{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hkl.XRD
       ( XRDRef(..)
       , XRDSample(..)
       , DataFrameH5(..)
       , DataFrameH5Path(..)
       , NxEntry
       , Nxs(..)
       , Bins(..)
       , Threshold(..)
       , XrdNxs(..)
       , PoniExt(..)
         -- methods
       , getPoniExtRef
       , integrate
       ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Concurrent.Async
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Morph (hoist)
import Data.Attoparsec.Text
import Data.ByteString.Char8 (pack)
import Data.Either
import Data.Text (Text, pack, unlines)
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
import System.Process
import Text.Printf (printf)

-- | Types

type NxEntry = String
type OutputBaseDir = FilePath
type PoniGenerator = MyMatrix Double -> Int -> IO PoniExt
type SampleName = String

data Bins = Bins Int
          deriving (Show)

data Threshold = Threshold Int
               deriving (Show)

data XRDRef = XRDRef SampleName OutputBaseDir Nxs Int

data XRDSample = XRDSample SampleName OutputBaseDir [XrdNxs]-- ^ nxss

data XrdNxs = XrdNxs Bins Threshold Nxs deriving (Show)

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
  return $ df_poniext (Prelude.last poniExtRefs)
  where
    gen :: FilePath -> FilePath -> MyMatrix Double -> Int -> IO PoniExt
    gen root nxs'' m idx' = do
      poni <- poniFromFile $ root </> scandir ++ printf "_%02d.poni" idx'
      return $ PoniExt poni m
      where
        scandir = takeFileName nxs''

integrate :: PoniExt -> XRDSample -> IO ()
integrate ref (XRDSample _ output nxss) = do
  _ <- mapConcurrently (integrate' ref output) nxss
  return ()

integrate' :: PoniExt -> OutputBaseDir -> XrdNxs -> IO ()
integrate' ref output (XrdNxs b t nxs'@(Nxs f _ _)) = do
  Prelude.print f
  withH5File f $ \h5file ->
      runSafeT $ runEffect $
        withDataFrameH5 h5file nxs' (gen ref) yield
        >-> hoist lift (frames
                        >-> savePonies (pgen output f)
                        >-> savePy b t)
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

createPy :: Bins -> Threshold -> (DifTomoFrame, FilePath) -> Text
createPy (Bins b) (Threshold t) (f, poniFileName) = Data.Text.unlines $
  map Data.Text.pack ["#!/bin/env python"
                     , ""
                     , "import numpy"
                     , "from h5py import File"
                     , "from pyFAI import load"
                     , ""
                     , "PONIFILE = " ++ show p
                     , "NEXUSFILE = " ++ show nxs'
                     , "IMAGEPATH = " ++ show i
                     , "IDX = " ++ show idx
                     , "N = " ++ show b
                     , "OUTPUT = " ++ show out
                     , "WAVELENGTH = " ++ show (w /~ meter)
                     , "THRESHOLD = " ++ show t
                     , ""
                     , "ai = load(PONIFILE)"
                     , "ai.wavelength = WAVELENGTH"
                     , "ai._empty = numpy.nan"
                     , "mask_det = ai.detector.mask"
                     , "#mask_module = numpy.zeros_like(mask_det)"
                     , "#mask_module[0:120, :] = True"
                     , "with File(NEXUSFILE) as f:"
                     , "    img = f[IMAGEPATH][IDX]"
                     , "    mask = numpy.where(img > THRESHOLD, True, False)"
                     , "    mask = numpy.logical_or(mask, mask_det)"
                     , "    #mask = numpy.logical_or(mask, mask_module)"
                     , "    ai.integrate1d(img, N, filename=OUTPUT, unit=\"2th_deg\", error_model=\"poisson\", correctSolidAngle=False, method=\"lut\", mask=mask)"
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
withDataFrameH5 h nxs'@(Nxs _ _ d) gen = Pipes.Safe.bracket (liftIO before) (liftIO . after)
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

savePy :: Bins -> Threshold -> Consumer (DifTomoFrame, FilePath) IO ()
savePy b t = forever $ do
  f@(_, poniFileName) <- await
  let directory = takeDirectory poniFileName
  let pyFileName = dropExtension poniFileName ++ ".py"
  lift $ createDirectoryIfMissing True directory
  lift $ writeFile pyFileName (createPy b t f)
  lift $ Prelude.print $ "--> " ++ pyFileName
  lift $ system (unwords ["cd ", directory, "&&",  "python", pyFileName])

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
