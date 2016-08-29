{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hkl.Diffabs.Martinetto
       ( main_martinetto ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Morph (hoist)
import Data.Attoparsec.Text
import Data.ByteString.Char8 (pack)
import Data.Char (toUpper)
import Data.Either
import Data.Text (Text, intercalate, pack)
import Data.Text.IO (readFile, writeFile)
import Data.Vector.Storable (concat, head)
import Hkl.C (geometryDetectorRotationGet)
import Hkl.H5 ( Dataset
              , File
              , closeDataset
              , get_position
              , lenH5Dataspace
              , openDataset
              , withH5File )
import Hkl.PyFAI
import Hkl.Types
import Numeric.Units.Dimensional.Prelude (meter, nano, (/~), (*~))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), dropExtension, takeFileName, takeDirectory)
import Prelude hiding (concat, lookup, readFile, writeFile)

import Pipes (Consumer, Pipe, lift, (>->), runEffect, await, yield)
import Pipes.Prelude (toListM)
import Pipes.Safe (MonadSafe(..), runSafeT, bracket)
import Text.Printf (printf)

-- | Types

type NxEntry = String
type OutputBaseDir = FilePath
type PoniGenerator = MyMatrix Double -> Int -> IO PoniExt
type SampleName = String

data XRDRef = XRDRef SampleName OutputBaseDir Nxs Int

data XRDSample = XRDSample SampleName OutputBaseDir [Nxs]-- ^ nxss

data Nxs = Nxs FilePath NxEntry DataFrameH5Path deriving (Show)

data PoniExt = PoniExt Poni (MyMatrix Double) deriving (Show)

data DifTomoFrame =
  DifTomoFrame { df_nxs :: Nxs -- ^ nexus of the current frame
               , df_n :: Int -- ^ index of the current frame
               , df_geometry :: Geometry -- ^ diffractometer geometry
               , df_poniext :: PoniExt -- ^ the ref poniext
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
    let nxs' = h5nxs d
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
    return DifTomoFrame { df_nxs = nxs'
                        , df_n = idx
                        , df_geometry = geometry
                        , df_poniext = poniext
                        }

-- | Samples

-- project = "/home/experiences/instrumentation/picca/data/99160066"
project :: FilePath
project = "/nfs/ruche-diffabs/diffabs-users/99160066/"
-- project = "/home/picca/data/99160066"

published :: FilePath
published = project </> "published-data"

beamlineUpper :: Beamline -> String
beamlineUpper b = [toUpper x | x <- show b]

nxs :: FilePath -> NxEntry -> (NxEntry -> DataFrameH5Path ) -> Nxs
nxs f e h = Nxs f e (h e)

calibration :: XRDRef
calibration = XRDRef "calibration"
              (published </> "calibration")
              (nxs (published </> "calibration" </> "XRD18keV_26.nxs") "scan_26" h5path')
              0
  where
    beamline :: String
    beamline = beamlineUpper Diffabs

    image = "scan_data/data_53"
    gamma = "d13-1-cx1__EX__DIF.1-GAMMA__#1/raw_value"
    delta = "scan_data/trajectory_1_1"
    wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

    h5path' :: NxEntry -> DataFrameH5Path
    h5path' nxentry =
      DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
                      , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
                      , h5pDelta = DataItem (nxentry </> delta) ExtendDims
                      , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
                      }


h5path :: NxEntry -> DataFrameH5Path
h5path nxentry =
  DataFrameH5Path { h5pImage = DataItem (nxentry </> image) StrictDims
                  , h5pGamma = DataItem (nxentry </> beamline </> gamma) ExtendDims
                  , h5pDelta = DataItem (nxentry </> delta) ExtendDims
                  , h5pWavelength = DataItem (nxentry </> beamline </> wavelength) StrictDims
                  }
  where
    beamline :: String
    beamline = beamlineUpper Diffabs

    image = "scan_data/data_53"
    gamma = "D13-1-CX1__EX__DIF.1-GAMMA__#1/raw_value"
    delta = "scan_data/trajectory_1_1"
    wavelength = "D13-1-C03__OP__MONO__#1/wavelength"

n27t2 :: XRDSample
n27t2 = XRDSample "N27T2"
        (published </> "N27T2")
        [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "N27T2_14.nxs") "scan_14" h5path
        , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "N27T2_17.nxs") "scan_17" h5path
        ]

r34n1 :: XRDSample
r34n1 = XRDSample "R34N1"
        (published </> "R34N1")
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R34N1_28.nxs") "scan_28" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R34N1_37.nxs") "scan_37" h5path
         ]

r23 :: XRDSample
r23 = XRDSample "R23"
        (published </> "R23")
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R23_6.nxs") "scan_6" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R23_12.nxs") "scan_12" h5path
         ]

r18 :: XRDSample
r18 = XRDSample "R18"
        (published </> "R18")
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R18_20.nxs") "scan_20" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R18_24.nxs") "scan_24" h5path
         ]

a3 :: XRDSample
a3 = XRDSample "A3"
        (published </> "A3")
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A3_13.nxs") "scan_13" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A3_14.nxs") "scan_14" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A3_15.nxs") "scan_15" h5path
         ]

a2 :: XRDSample
a2 = XRDSample "A2"
        (published </> "A2")
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A2_14.nxs") "scan_14" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "A2_17.nxs") "scan_17" h5path
         ]

d2 :: XRDSample
d2 = XRDSample "D2"
        (published </> "D2")
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D2_16.nxs") "scan_16" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D2_17.nxs") "scan_17" h5path
         ]

d3 :: XRDSample
d3 = XRDSample "D3"
        (published </> "D3")
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D3_14.nxs") "scan_14" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D3_15.nxs") "scan_15" h5path
         ]

r11 :: XRDSample
r11 = XRDSample "R11"
        (published </> "R11")
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R11_5.nxs") "scan_5" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R11_6.nxs") "scan_6" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "R11_7.nxs") "scan_7" h5path
         ]

d16 :: XRDSample
d16 = XRDSample "D16"
        (published </> "D16")
         [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D16_12.nxs") "scan_12" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D16_15.nxs") "scan_15" h5path
         , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "D16_17.nxs") "scan_17" h5path
         ]

k9a2 :: XRDSample
k9a2 = XRDSample "K9A2"
       (published </> "K9A2")
       [ nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "K9A2_1_31.nxs") "scan_31" h5path
       , nxs (project </> "2016" </> "Run2" </> "2016-03-27" </> "K9A2_1_32.nxs") "scan_32" h5path
       ]

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


poniFromFile :: FilePath -> IO Poni
poniFromFile filename = do
  content <- readFile filename
  return $ case parseOnly poniP content of
    Left _     -> error $ "Can not parse the " ++ filename ++ " poni file"
    Right poni -> poni

getPoniExtRef :: XRDRef -> IO PoniExt
getPoniExtRef (XRDRef _ output nxs'@(Nxs f _ _) idx) = do
  poniExtRefs <- withH5File f $ \h5file ->
    runSafeT $ toListM ( withDataFrameH5 h5file nxs' (gen output f) yield
                         >-> hoist lift (frames' [idx]))
  return $ df_poniext (Prelude.head poniExtRefs)
  where
    gen :: FilePath -> FilePath -> MyMatrix Double -> Int -> IO PoniExt
    gen root nxs'' m idx' = do
      poni <- poniFromFile $ root </> scandir ++ printf "_%02d.poni" idx'
      return $ PoniExt poni m
      where
        scandir = takeFileName nxs''

integrate :: PoniExt -> XRDSample -> IO ()
integrate ref (XRDSample _ output nxss) = mapM_ (integrate' ref output) nxss

integrate' :: PoniExt -> OutputBaseDir -> Nxs -> IO ()
integrate' ref output nxs'@(Nxs f _ _) = do
  Prelude.print f
  withH5File f $ \h5file ->
      runSafeT $ runEffect $
        withDataFrameH5 h5file nxs' (gen ref) yield
        >-> hoist lift (frames
                        >-> savePonies (pgen output f)
                        >-> savePy 300)
  where
    gen :: PoniExt -> MyMatrix Double -> Int -> IO PoniExt
    gen ref' m _idx = return $ computeNewPoni ref' m

    pgen :: OutputBaseDir -> FilePath -> Int -> FilePath
    pgen o nxs'' idx = o </> scandir </>  scandir ++ printf "_%02d.poni" idx
      where
        scandir = (dropExtension . takeFileName) nxs''

computeNewPoni :: PoniExt -> MyMatrix Double -> PoniExt
computeNewPoni (PoniExt p1 mym1) mym2 = PoniExt p2 mym2
  where
    p2 = map rotate p1

    rotate :: PoniEntry -> PoniEntry
    rotate e = rotatePoniEntry e mym1 mym2

createPy :: Int -> (DifTomoFrame, FilePath) -> Text
createPy nb (f, poniFileName) =
  intercalate "\n" $
  map Data.Text.pack ["#!/bin/env python"
                     , ""
                     , "from h5py import File"
                     , "from pyFAI import load"
                     , ""
                     , "PONIFILE = " ++ show p
                     , "NEXUSFILE = " ++ show nxs'
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
    (Nxs nxs' _ h5path') = df_nxs f
    (DataItem i _) = h5pImage h5path'
    idx = df_n f
    out = (dropExtension . takeFileName) poniFileName ++ ".dat"
    (Geometry _ (Source w) _ _) = df_geometry f

-- | Pipes

withDataFrameH5 :: (MonadSafe m) => File -> Nxs -> PoniGenerator -> (DataFrameH5 -> m r) -> m r
withDataFrameH5 h nxs'@(Nxs _ _ d) gen = Pipes.Safe.bracket (liftIO $ before) (liftIO . after)
  where
    -- before :: File -> DataFrameH5Path -> m DataFrameH5
    before :: IO DataFrameH5
    before =  DataFrameH5
              <$> return nxs'
              <*> openDataset' h (h5pGamma d)
              <*> openDataset' h (h5pDelta d)
              <*> openDataset' h (h5pWavelength d)
              <*> return gen

    -- after :: DataFrameH5 -> IO ()
    after d' = do
      closeDataset (h5gamma d')
      closeDataset (h5delta d')
      closeDataset (h5wavelength d')

    -- openDataset' :: File -> DataItem -> IO Dataset
    openDataset' hid (DataItem name _) = openDataset hid (Data.ByteString.Char8.pack name) Nothing

savePonies :: (Int -> FilePath) -> Pipe DifTomoFrame (DifTomoFrame, FilePath) IO ()
savePonies g = forever $ do
  f <- await
  let filename = g (df_n f)
  lift $ createDirectoryIfMissing True (takeDirectory filename)
  lift $ writeFile filename (content (df_poniext f))
  lift $ Prelude.print $ "--> " ++ filename
  yield (f, filename)
    where
      content :: PoniExt -> Text
      content (PoniExt poni _) = poniToText poni

savePy :: Int-> Consumer (DifTomoFrame, FilePath) IO ()
savePy n = forever $ do
  f@(_, poniFileName) <- await
  let directory = takeDirectory poniFileName
  let pyFileName = dropExtension poniFileName ++ ".py"
  lift $ createDirectoryIfMissing True directory
  lift $ writeFile pyFileName (createPy n f)
  lift $ Prelude.print $ "--> " ++ pyFileName

frames :: (Frame a) => Pipe a DifTomoFrame IO ()
frames = do
  d <- await
  (Just n) <- lift $ len d
  forM_ [0..n-1] (\i -> do
                     f <- lift $ row d i
                     yield f)

frames' :: (Frame a) => [Int] -> Pipe a DifTomoFrame IO ()
frames' idxs = do
  d <- await
  forM_ idxs (\i -> do
                 f <- lift $ row d i
                 yield f)

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
