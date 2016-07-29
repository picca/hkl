{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hkl.Diffabs.Martinetto
       ( main_martinetto ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Exception (bracket)
import Control.Monad (forM_, forever, liftM)
import Data.Attoparsec.Text
import Data.ByteString.Char8 (pack)
import Data.Char (toUpper)
import Data.Either
import Data.List (sort)
import Numeric.LinearAlgebra hiding (row)
import Data.Text (Text, intercalate, pack)
import Data.Text.IO (hPutStrLn, readFile, writeFile)
import Data.Vector.Storable (concat, head)
import Hkl.C (geometryDetectorRotationGet)
import Hkl.H5 ( Dataset
              , File
              , check_ndims
              , closeDataset
              , get_position
              , lenH5Dataspace
              , openDataset
              , withH5File )
import Hkl.PyFAI
import Hkl.Types
import Numeric.Units.Dimensional.Prelude (meter, degree, nano, (/~), (*~))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), dropExtension, takeFileName, takeDirectory)
import System.FilePath.Glob (glob)
import System.IO (withFile, IOMode(WriteMode) )

import Prelude hiding (concat, lookup, readFile, writeFile)

import Pipes (Consumer, Pipe, Producer, lift, (>->), runEffect, await, yield)
import Pipes.Prelude (toListM, drain, print)
import Text.Printf (printf)

-- | Types

type NxEntry = String
type OutputBaseDir = FilePath
type PoniGenerator = MyMatrix Double -> Int -> IO PoniExt
type SampleName = String

data XRFRef = XRFRef SampleName OutputBaseDir Nxs Int

data XRFSample = XRFSample SampleName OutputBaseDir [Nxs]-- ^ nxss

data Nxs = Nxs FilePath NxEntry DataFrameH5Path deriving (Show)

data PoniExt = PoniExt Poni (MyMatrix Double) deriving (Show)

data DifTomoFrame =
  DifTomoFrame { df_nxs :: Nxs -- ^ nexus of the current frame
               , df_n :: Int -- ^ index of the current frame
               , df_geometry :: Geometry -- ^ diffractometer geometry
               , df_poniext :: PoniExt -- ^ the ref poniext
               } deriving (Show)

data DifTomoFrame2 =
  DifTomoFrame2 { df_frame :: DifTomoFrame
                , df_poni_filename :: FilePath
                } deriving (Show)

class Frame t where
  len :: t -> IO (Maybe Int)
  row :: t -> Int -> IO DifTomoFrame

data DataFrameH5Path =
  DataFrameH5Path { h5pImage :: DataItem
                  , h5pGamma :: DataItem
                  , h5pDelta :: DataItem
                  , h5pWavelength :: DataItem
                  } deriving (Show)

data DataFrameH5 =
  DataFrameH5 { h5nxs :: Nxs
              , h5gamma :: Dataset
              , h5delta :: Dataset
              , h5wavelength :: Dataset
              , ponigen :: PoniGenerator
              }

instance Frame DataFrameH5 where
  len d =  lenH5Dataspace (h5delta d)

  row d idx = do
    let nxs = h5nxs d
    let mu = 0.0
    let komega = 0.0
    let kappa = 0.0
    let kphi = 0.0
    gamma <- get_position (h5gamma d) 0
    delta <- get_position (h5delta d) idx
    wavelength <- get_position (h5wavelength d) 0
    let source = Source (Data.Vector.Storable.head wavelength *~ nano meter)
    let positions = concat [mu, komega, kappa, kphi, gamma, delta]
    let geometry =  Geometry K6c source positions Nothing
    let detector = Detector DetectorType0D
    m <- geometryDetectorRotationGet geometry detector
    poniext <- ponigen d (MyMatrix HklB m) idx
    return DifTomoFrame { df_nxs = nxs
                        , df_n = idx
                        , df_geometry = geometry
                        , df_poniext = poniext
                        }

-- | Samples

-- project = "/nfs/ruche-diffabs/diffabs-users/99160066/"
-- project = "/home/experiences/instrumentation/picca/data/99160066"
project = "/home/picca/data/99160066"
published = project </> "published-data"

beamlineUpper :: Beamline -> String
beamlineUpper b = [toUpper x | x <- show b]

nxs :: FilePath -> NxEntry -> (NxEntry -> DataFrameH5Path ) -> Nxs
nxs f e h5path = Nxs f e (h5path e)


calibration :: XRFRef
calibration = XRFRef "calibration"
              (published </> "calibration")
              (nxs (published </> "calibration" </> "XRD18keV_26.nxs") "scan_26" h5path)
              0
  where
    beamline :: String
    beamline = beamlineUpper Diffabs

    image = "scan_data/data_53"
    gamma = "d13-1-cx1__EX__DIF.1-GAMMA__#1/raw_value"
    delta = "scan_data/trajectory_1_1"
    wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

    h5path :: NxEntry -> DataFrameH5Path
    h5path nxentry =
      DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
                      , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
                      , h5pDelta = DataItem (nxentry </> delta) ExtendDims
                      , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
                      }

n27t2 = XRFSample "N27T2"
        (published </> "N27T2")
        [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "N27T2_14.nxs") "scan_14" h5path
        , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "N27T2_17.nxs") "scan_17" h5path
        ]
  where
    beamline :: String
    beamline = beamlineUpper Diffabs

    image = "scan_data/data_53"
    gamma = "D13-1-CX1__EX__DIF.1-GAMMA__#1/raw_value"
    delta = "scan_data/trajectory_1_1"
    wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

    h5path :: NxEntry -> DataFrameH5Path
    h5path nxentry =
      DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
                      , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
                      , h5pDelta = DataItem (nxentry </> delta) ExtendDims
                      , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
                      }

r34n1 = XRFSample "R34N1"
        (published </> "R34N1")
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R34N1_28.nxs") "scan_28" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R34N1_37.nxs") "scan_37" h5path
         ]
  where
    beamline :: String
    beamline = beamlineUpper Diffabs

    image = "scan_data/data_53"
    gamma = "D13-1-CX1__EX__DIF.1-GAMMA__#1/raw_value"
    delta = "scan_data/trajectory_1_1"
    wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

    h5path :: NxEntry -> DataFrameH5Path
    h5path nxentry =
      DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
                      , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
                      , h5pDelta = DataItem (nxentry </> delta) ExtendDims
                      , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
                      }

r23 = XRFSample "R23"
        (published </> "R23")
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R23_6.nxs") "scan_6" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R23_12.nxs") "scan_12" h5path
         ]
  where
    beamline :: String
    beamline = beamlineUpper Diffabs

    image = "scan_data/data_53"
    gamma = "D13-1-CX1__EX__DIF.1-GAMMA__#1/raw_value"
    delta = "scan_data/trajectory_1_1"
    wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

    h5path :: NxEntry -> DataFrameH5Path
    h5path nxentry =
      DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
                      , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
                      , h5pDelta = DataItem (nxentry </> delta) ExtendDims
                      , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
                      }

r18 = XRFSample "R18"
        (published </> "R18")
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R18_20.nxs") "scan_20" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R18_24.nxs") "scan_24" h5path
         ]
  where
    beamline :: String
    beamline = beamlineUpper Diffabs

    image = "scan_data/data_53"
    gamma = "D13-1-CX1__EX__DIF.1-GAMMA__#1/raw_value"
    delta = "scan_data/trajectory_1_1"
    wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

    h5path :: NxEntry -> DataFrameH5Path
    h5path nxentry =
      DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
                      , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
                      , h5pDelta = DataItem (nxentry </> delta) ExtendDims
                      , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
                      }

a3 = XRFSample "A3"
        (published </> "A3")
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A3_13.nxs") "scan_13" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A3_14.nxs") "scan_14" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A3_15.nxs") "scan_15" h5path
         ]
  where
    beamline :: String
    beamline = beamlineUpper Diffabs

    image = "scan_data/data_53"
    gamma = "D13-1-CX1__EX__DIF.1-GAMMA__#1/raw_value"
    delta = "scan_data/trajectory_1_1"
    wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

    h5path :: NxEntry -> DataFrameH5Path
    h5path nxentry =
      DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
                      , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
                      , h5pDelta = DataItem (nxentry </> delta) ExtendDims
                      , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
                      }

a2 = XRFSample "A2"
        (published </> "A2")
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A2_14.nxs") "scan_14" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A2_17.nxs") "scan_17" h5path
         ]
  where
    beamline :: String
    beamline = beamlineUpper Diffabs

    image = "scan_data/data_53"
    gamma = "D13-1-CX1__EX__DIF.1-GAMMA__#1/raw_value"
    delta = "scan_data/trajectory_1_1"
    wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

    h5path :: NxEntry -> DataFrameH5Path
    h5path nxentry =
      DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
                      , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
                      , h5pDelta = DataItem (nxentry </> delta) ExtendDims
                      , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
                      }

d2 = XRFSample "D2"
        (published </> "D2")
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D2_16.nxs") "scan_16" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D2_17.nxs") "scan_17" h5path
         ]
  where
    beamline :: String
    beamline = beamlineUpper Diffabs

    image = "scan_data/data_53"
    gamma = "D13-1-CX1__EX__DIF.1-GAMMA__#1/raw_value"
    delta = "scan_data/trajectory_1_1"
    wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

    h5path :: NxEntry -> DataFrameH5Path
    h5path nxentry =
      DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
                      , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
                      , h5pDelta = DataItem (nxentry </> delta) ExtendDims
                      , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
                      }

d3 = XRFSample "D3"
        (published </> "D3")
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D3_14.nxs") "scan_14" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D3_15.nxs") "scan_15" h5path
         ]
  where
    beamline :: String
    beamline = beamlineUpper Diffabs

    image = "scan_data/data_53"
    gamma = "D13-1-CX1__EX__DIF.1-GAMMA__#1/raw_value"
    delta = "scan_data/trajectory_1_1"
    wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

    h5path :: NxEntry -> DataFrameH5Path
    h5path nxentry =
      DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
                      , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
                      , h5pDelta = DataItem (nxentry </> delta) ExtendDims
                      , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
                      }

r11 = XRFSample "R11"
        (published </> "R11")
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R11_5.nxs") "scan_5" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R11_6.nxs") "scan_6" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R11_7.nxs") "scan_7" h5path
         ]
  where
    beamline :: String
    beamline = beamlineUpper Diffabs

    image = "scan_data/data_53"
    gamma = "D13-1-CX1__EX__DIF.1-GAMMA__#1/raw_value"
    delta = "scan_data/trajectory_1_1"
    wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

    h5path :: NxEntry -> DataFrameH5Path
    h5path nxentry =
      DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
                      , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
                      , h5pDelta = DataItem (nxentry </> delta) ExtendDims
                      , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
                      }

d16 = XRFSample "D16"
        (published </> "D16")
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D16_12.nxs") "scan_12" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D16_15.nxs") "scan_15" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D16_17.nxs") "scan_17" h5path
         ]
  where
    beamline :: String
    beamline = beamlineUpper Diffabs

    image = "scan_data/data_53"
    gamma = "D13-1-CX1__EX__DIF.1-GAMMA__#1/raw_value"
    delta = "scan_data/trajectory_1_1"
    wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

    h5path :: NxEntry -> DataFrameH5Path
    h5path nxentry =
      DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
                      , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
                      , h5pDelta = DataItem (nxentry </> delta) ExtendDims
                      , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
                      }

k9a2 = XRFSample "K9A2"
       (published </> "K9A2")
       [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "K9A2_1_31.nxs") "scan_31" h5path
       , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "K9A2_1_32.nxs") "scan_32" h5path
       ]
  where
    beamline :: String
    beamline = beamlineUpper Diffabs

    image = "scan_data/data_53"
    gamma = "D13-1-CX1__EX__DIF.1-GAMMA__#1/raw_value"
    delta = "scan_data/trajectory_1_1"
    wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

    h5path :: NxEntry -> DataFrameH5Path
    h5path nxentry =
      DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
                      , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
                      , h5pDelta = DataItem (nxentry </> delta) ExtendDims
                      , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
                      }

-- {-# ANN module "HLint: ignore Use camelCase" #-}


-- import Graphics.Rendering.Chart.Easy
-- import Graphics.Rendering.Chart.Backend.Diagrams

-- plotPonies :: FilePath -> [PoniEntry] -> IO ()
-- plotPonies f entries = toFile def f $ do
--     layout_title .= "Ponies"
--     setColors [opaque blue]
--     let values = map extract entries
--     plot (line "am" [values [0,(0.5)..400]])
--     -- plot (points "am points" (signal [0,7..400]))
--     where
--       extract (PoniEntry _ _ (Length poni1) _ _ _ _ _ _) = poni1

-- | Usual methods

title :: Text
title = intercalate "\t" [ "# distance"
                         , "poni1"
                         , "poni2"
                         , "rot1"
                         , "rot2"
                         , "rot3" ]

toText :: PoniEntry -> Text
toText (PoniEntry _ _ _ _ d p1 p2 rot1 rot2 rot3 _ _) =
  intercalate "\t"
  (map (Data.Text.pack . show) [ d /~ meter
                               , p1 /~ meter
                               , p2 /~ meter
                               , rot1 /~ degree
                               , rot2 /~ degree
                               , rot3 /~ degree])

save :: FilePath -> [PoniEntry] -> IO ()
save f ps = withFile f WriteMode $ \handler -> do
  hPutStrLn handler title
  mapM_ (put handler) ps
    where
      put h p = hPutStrLn h (toText p)

poniFromFile :: FilePath -> IO Poni
poniFromFile filename = do
  content <- readFile filename
  return $ case parseOnly poniP content of
    Left _     -> error $ "Can not parse the " ++ filename ++ " poni file"
    Right poni -> poni

ponies :: [FilePath] -> IO [PoniEntry]
ponies fs = mapM extract (sort fs)
  where
     extract :: FilePath -> IO PoniEntry
     extract filename = liftM last (poniFromFile filename)

withDataframeH5 :: File -> DataFrameH5Path -> PoniGenerator -> Nxs -> (DataFrameH5 -> IO r) -> IO r
withDataframeH5 h5file dfp gen nxs = bracket (acquire h5file dfp) release
  where
    acquire :: File -> DataFrameH5Path -> IO DataFrameH5
    acquire h d =  DataFrameH5
                   <$> return nxs
                   <*> openDataset' h (h5pGamma d)
                   <*> openDataset' h (h5pDelta d)
                   <*> openDataset' h (h5pWavelength d)
                   <*> return gen

    release :: DataFrameH5 -> IO ()
    release d = do
      closeDataset (h5gamma d)
      closeDataset (h5delta d)
      closeDataset (h5wavelength d)

    openDataset' :: File -> DataItem -> IO Dataset
    openDataset' hid (DataItem name _) = openDataset hid (Data.ByteString.Char8.pack name) Nothing


hkl_h5_is_valid :: DataFrameH5-> IO Bool
hkl_h5_is_valid d = do
  True <- check_ndims (h5gamma d) 1
  True <- check_ndims (h5delta d) 1
  return True

plotPoni :: String -> FilePath -> IO ()
plotPoni pattern output = do
  filenames <- glob pattern
  -- print $ sort filenames
  -- print $ [0,3..39 :: Int] ++ [43 :: Int]
  -- print nxs
  entries <- ponies filenames
  save output entries


getPoniExtRef :: XRFRef -> IO PoniExt
getPoniExtRef (XRFRef _ output nxs@(Nxs f e h5path) idx) = do
  poniExtRefs <- withH5File f $ \h5file ->
    withDataframeH5 h5file h5path (gen output f) nxs $ \dataframe_h5 ->
      toListM (frames' dataframe_h5 [idx])
  return $ df_poniext (Prelude.head poniExtRefs)
  where
    gen :: FilePath -> FilePath -> MyMatrix Double -> Int -> IO PoniExt
    gen root nxs m idx = do
      poni <- poniFromFile $ root </> scandir ++ printf "_%02d.poni" idx
      return $ PoniExt poni m
      where
        scandir = takeFileName nxs

integrate :: PoniExt -> XRFSample -> IO ()
integrate ref (XRFSample _ output nxss) = mapM_ (integrate' ref output) nxss

integrate' :: PoniExt -> OutputBaseDir -> Nxs -> IO ()
integrate' ref output nxs@(Nxs f e h5path) = do
  Prelude.print f
  withH5File f $ \h5file ->
    withDataframeH5 h5file h5path (gen ref) nxs $ \dataframe_h5 -> do
      True <- hkl_h5_is_valid dataframe_h5

      runEffect $ frames dataframe_h5
        >-> savePonies (pgen output f)
        >-> savePy 300
        >-> drain
  where
    gen :: PoniExt -> MyMatrix Double -> Int -> IO PoniExt
    gen ref m _idx = return $ computeNewPoni ref m

    pgen :: OutputBaseDir -> FilePath -> Int -> FilePath
    pgen o nxs idx = o </> scandir </>  scandir ++ printf "_%02d.poni" idx
      where
        scandir = (dropExtension . takeFileName) nxs

computeNewPoni :: PoniExt -> MyMatrix Double -> PoniExt
computeNewPoni (PoniExt p1 mym1) mym2 = PoniExt p2 mym2
  where
    p2 = map rotate p1

    rotate :: PoniEntry -> PoniEntry
    rotate e = rotatePoniEntry e mym1 mym2

createPy :: Int -> DifTomoFrame2 -> Text
createPy nb f2@(DifTomoFrame2 f poniFileName) =
  intercalate "\n" $
  map Data.Text.pack ["#!/bin/env python"
                     , ""
                     , "from h5py import File"
                     , "from pyFAI import load"
                     , ""
                     , "PONIFILE = " ++ show p
                     , "NEXUSFILE = " ++ show nxs
                     , "IMAGEPATH = " ++ show i
                     , "IDX = " ++ show idx
                     , "N = " ++ show nb
                     , "OUTPUT = " ++ show out
                     , "WAVELENGTH = " ++ show (w /~ meter)
                     , ""
                     , "ai = load(PONIFILE)"
                     , "ai.wavelength = WAVELENGTH"
                     , "with File(NEXUSFILE) as f:"
                     , "    img = f[IMAGEPATH][IDX]"
                     , "    ai.integrate1d(img, N, filename=OUTPUT)"
                     ]
  where
    p = takeFileName poniFileName
    (Nxs nxs nxentry h5path) = df_nxs f
    (DataItem i _) = h5pImage h5path
    idx = df_n f
    out = (dropExtension . takeFileName) poniFileName ++ ".dat"
    (Geometry _ (Source w) _ _) = df_geometry f

-- | Pipes

savePonies :: (Int -> FilePath) -> Pipe DifTomoFrame DifTomoFrame2 IO ()
savePonies g = forever $ do
  f <- await
  let filename = g (df_n f)
  lift $ createDirectoryIfMissing True (takeDirectory filename)
  lift $ writeFile filename (content (df_poniext f))
  lift $ Prelude.print $ "--> " ++ filename
  yield $ DifTomoFrame2 f filename
    where
      content :: PoniExt -> Text
      content (PoniExt poni _) = poniToText poni

savePy :: Int-> Pipe DifTomoFrame2 DifTomoFrame2 IO ()
savePy n = forever $ do
  f2@(DifTomoFrame2 _ poniFileName) <- await
  let directory = takeDirectory poniFileName
  let pyFileName = (dropExtension poniFileName) ++ ".py"
  lift $ createDirectoryIfMissing True directory
  lift $ writeFile pyFileName (createPy n f2)
  lift $ Prelude.print $ "--> " ++ pyFileName
  yield f2

frames :: Frame a => a -> Producer DifTomoFrame IO ()
frames d = do
  (Just n) <- lift $ len d
  frames' d [0..n-1]

frames' :: Frame a => a -> [Int] -> Producer DifTomoFrame IO ()
frames' d idxs = forM_ idxs (\i -> lift (row d i) >>= yield)


-- | Main

main_martinetto :: IO ()
main_martinetto = do
  -- lire le ou les ponis de référence ainsi que leur géométrie
  -- associée.

  let samples = [n27t2, r34n1, r23, r18, a2, a3, d2, d3, r11, d16, k9a2]

  poniextref <- getPoniExtRef calibration

  -- calculer et écrire pour chaque point d'un scan un poni correspondant à la bonne géométries.

  mapM_ (integrate poniextref) samples

  -- plotPoni "/tmp/*.poni" "/tmp/plot.txt"

  -- créer le script python d'intégration multi géométrie

  -- l'executer pour faire l'intégration.

  -- plot de la figure. (script python ou autre ?)
