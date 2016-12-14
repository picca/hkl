{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Hkl.XRD
       ( XRDRef(..)
       , XrdRefSource(..)
       , XRDSample(..)
       , DataFrameH5(..)
       , DataFrameH5Path(..)
       , NxEntry
       , Nxs(..)
       , Threshold(..)
       , XrdNxs(..)
       , XrdSource(..)
       , PoniExt(..)
         -- reference
       , getMEdf
       , getPoniExtRef
         -- integration
       , integrate
       , integrateMulti
       -- Mesh
       -- , DataFrameMeshH5Path(..)
       -- , Nxs'(..)
       -- , XrdNxs'(..)
       -- , XRDSample'(..)
       -- , integrateMesh
       ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_, forever, zipWithM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.State.Strict (StateT, get, put)
import Data.Array.Repa (Shape, DIM1, size)
import Data.Attoparsec.Text (parseOnly)
import qualified Data.ByteString.Char8 as Char8 (pack)
import qualified Data.List as List (intercalate, lookup)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text (unlines, pack, intercalate)
import Data.Text.IO (readFile)
import Data.Vector.Storable (concat, head)
import Numeric.LinearAlgebra (fromList)
import Numeric.Units.Dimensional.Prelude (meter, nano, (/~), (*~))
import System.Exit ( ExitCode( ExitSuccess ) )
import System.FilePath ((</>), dropExtension, takeFileName, takeDirectory)
import System.Process ( system )
import Text.Printf ( printf )

import Prelude hiding
    ( concat
    , head
    , lookup
    , readFile
    , unlines
    )
import Pipes
    ( Consumer
    , Pipe
    , lift
    , (>->)
    , runEffect
    , await
    , yield
    )
import Pipes.Lift
import Pipes.Prelude (toListM)
import Pipes.Safe ( MonadSafe(..), runSafeT, bracket )

import Hkl.C
import Hkl.Detector
import Hkl.Edf
import Hkl.H5
import Hkl.PyFAI
import Hkl.MyMatrix
import Hkl.PyFAI.PoniExt
import Hkl.Types
import Hkl.Utils

-- | Types

type NxEntry = String
type OutputBaseDir = FilePath
type PoniGenerator = Pose -> Int -> IO PoniExt
type SampleName = String

data Threshold = Threshold Int
               deriving (Show)

data XrdRefSource = XrdRefNxs Nxs Int
                  | XrdRefEdf FilePath FilePath
                    deriving (Show)

data XRDRef = XRDRef SampleName OutputBaseDir XrdRefSource
            deriving (Show)

data XRDSample = XRDSample SampleName OutputBaseDir [XrdNxs] -- ^ nxss
               deriving (Show)

data XrdSource = XrdSourceNxs Nxs
               | XrdSourceEdf [FilePath]
                 deriving (Show)

data XrdNxs = XrdNxs DIM1 DIM1 Threshold XrdSource deriving (Show)

data Nxs = Nxs FilePath NxEntry DataFrameH5Path deriving (Show)

data DifTomoFrame sh =
  DifTomoFrame { difTomoFrameNxs :: Nxs -- ^ nexus of the current frame
               , difTomoFrameIdx :: Int -- ^ index of the current frame
               , difTomoFrameEOF :: Bool -- ^ is it the eof of the stream
               , difTomoFrameGeometry :: Geometry -- ^ diffractometer geometry
               , difTomoFramePoniExt :: PoniExt -- ^ the ref poniext
               } deriving (Show)

class Frame t where
  len :: t -> IO (Maybe Int)
  row :: t -> Int -> IO (DifTomoFrame DIM1)

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
    (Just n) <- len d
    let eof = n - 1 == idx
    let nxs' = h5nxs d
    let mu = 0.0
    let komega = 0.0
    let kappa = 0.0
    let kphi = 0.0
    gamma <- get_position (h5gamma d) 0
    delta <- get_position (h5delta d) idx
    wavelength <- get_position (h5wavelength d) 0
    let source = Source (head wavelength *~ nano meter)
    let positions = concat [mu, komega, kappa, kphi, gamma, delta]
    let geometry =  Geometry K6c source positions Nothing
    let detector = ZeroD
    m <- geometryDetectorRotationGet geometry detector
    poniext <- ponigen d (MyMatrix HklB m) idx
    return DifTomoFrame { difTomoFrameNxs = nxs'
                        , difTomoFrameIdx = idx
                        , difTomoFrameEOF = eof
                        , difTomoFrameGeometry = geometry
                        , difTomoFramePoniExt = poniext
                        }

frames :: (Frame a) => Pipe a (DifTomoFrame DIM1) IO ()
frames = do
  d <- await
  (Just n) <- lift $ len d
  forM_ [0..n-1] (\i' -> do
                     f <- lift $ row d i'
                     yield f)

frames' :: (Frame a) => [Int] -> Pipe a (DifTomoFrame DIM1) IO ()
frames' is = do
  d <- await
  forM_ is (\i' -> do
              f <- lift $ row d i'
              yield f)

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

getMEdf :: FilePath -> IO (MyMatrix Double)
getMEdf f = do
  edf <- edfFromFile f
  let mnes = map Text.pack ["_mu", "_keta", "_kap", "_kphi", "nu", "del"]
  let source = Source (edf'Lambda edf)
  let positions = fromList $ map (extract edf) mnes
  let geometry =  Geometry K6c source positions Nothing
  let detector = ZeroD
  m <- geometryDetectorRotationGet geometry detector
  return $ MyMatrix HklB m
    where
      extract :: Edf -> Text -> Double
      extract (Edf _ ms) key = fromMaybe 0.0 (List.lookup key ms)

poniFromFile :: FilePath -> IO Poni
poniFromFile filename = do
  content <- readFile filename
  return $ case parseOnly poniP content of
    Left _     -> error $ "Can not parse the " ++ filename ++ " poni file"
    Right poni -> poni

getPoniExtRef :: XRDRef -> IO PoniExt
getPoniExtRef (XRDRef _ output (XrdRefNxs nxs'@(Nxs f _ _) idx)) = do
  poniExtRefs <- withH5File f $ \h5file ->
    runSafeT $ toListM ( withDataFrameH5 h5file nxs' (gen output f) yield
                         >-> hoist lift (frames' [idx]))
  return $ difTomoFramePoniExt (Prelude.last poniExtRefs)
  where
    gen :: FilePath -> FilePath -> MyMatrix Double -> Int -> IO PoniExt
    gen root nxs'' m idx' = do
      poni <- poniFromFile $ root </> scandir ++ printf "_%02d.poni" idx'
      return $ PoniExt poni m
      where
        scandir = takeFileName nxs''
getPoniExtRef (XRDRef _ _ (XrdRefEdf e p)) = do
  poni <- poniFromFile p
  m <- getMEdf e
  return $ PoniExt poni m

integrate :: PoniExt -> XRDSample -> IO ()
integrate ref (XRDSample _ output nxss) = do
  _ <- mapConcurrently (integrate' ref output) nxss
  return ()

integrate' :: PoniExt -> OutputBaseDir -> XrdNxs -> IO ()
integrate' ref output (XrdNxs b _ t (XrdSourceNxs nxs'@(Nxs f _ _))) = do
  print f
  withH5File f $ \h5file ->
      runSafeT $ runEffect $
        withDataFrameH5 h5file nxs' (gen ref) yield
        >-> hoist lift (frames
                        >-> savePonies (pgen output f)
                        >-> savePy b t
                        >-> saveGnuplot)
  where
    gen :: PoniExt -> Pose -> Int -> IO PoniExt
    gen ref' m _idx = return $ setPose ref' m

    pgen :: OutputBaseDir -> FilePath -> Int -> FilePath
    pgen o nxs'' idx = o </> scandir </>  scandir ++ printf "_%02d.poni" idx
      where
        scandir = (dropExtension . takeFileName) nxs''

createPy :: (Shape sh) => DIM1 -> Threshold -> (DifTomoFrame' sh) -> (Text, FilePath)
createPy b (Threshold t) (DifTomoFrame' f poniPath) = (script, output)
    where
      script = Text.unlines $
               map Text.pack ["#!/bin/env python"
                             , ""
                             , "import numpy"
                             , "from h5py import File"
                             , "from pyFAI import load"
                             , ""
                             , "PONIFILE = " ++ show p
                             , "NEXUSFILE = " ++ show nxs'
                             , "IMAGEPATH = " ++ show i'
                             , "IDX = " ++ show idx
                             , "N = " ++ show (size b)
                             , "OUTPUT = " ++ show output
                             , "WAVELENGTH = " ++ show (w /~ meter)
                             , "THRESHOLD = " ++ show t
                             , ""
                             , "ai = load(PONIFILE)"
                             , "ai.wavelength = WAVELENGTH"
                             , "ai._empty = numpy.nan"
                             , "mask_det = ai.detector.mask"
                             , "mask_module = numpy.zeros_like(mask_det, dtype=bool)"
                             , "mask_module[0:50, :] = True"
                             , "mask_module[910:960, :] = True"
                             , "mask_module[:,0:50] = True"
                             , "mask_module[:,510:560] = True"
                             , "with File(NEXUSFILE, mode='r') as f:"
                             , "    img = f[IMAGEPATH][IDX]"
                             , "    mask = numpy.where(img > THRESHOLD, True, False)"
                             , "    mask = numpy.logical_or(mask, mask_det)"
                             , "    mask = numpy.logical_or(mask, mask_module)"
                             , "    ai.integrate1d(img, N, filename=OUTPUT, unit=\"2th_deg\", error_model=\"poisson\", correctSolidAngle=False, method=\"lut\", mask=mask)"
                                  ]
      p = takeFileName poniPath
      (Nxs nxs' _ h5path') = difTomoFrameNxs f
      (DataItem i' _) = h5pImage h5path'
      idx = difTomoFrameIdx f
      output = (dropExtension . takeFileName) poniPath ++ ".dat"
      (Geometry _ (Source w) _ _) = difTomoFrameGeometry f

-- | Pipes

withDataFrameH5 :: (MonadSafe m) => File -> Nxs -> PoniGenerator -> (DataFrameH5 -> m r) -> m r
withDataFrameH5 h nxs'@(Nxs _ _ d) gen = bracket (liftIO before) (liftIO . after)
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
    openDataset' hid (DataItem name _) = openDataset hid (Char8.pack name) Nothing

data DifTomoFrame' sh = DifTomoFrame' { difTomoFrame'DifTomoFrame :: DifTomoFrame sh
                                      , difTomoFrame'PoniPath :: FilePath
                                      }

savePonies :: (Shape sh) => (Int -> FilePath) -> Pipe (DifTomoFrame sh) (DifTomoFrame' sh) IO ()
savePonies g = forever $ do
  f <- await
  let filename = g (difTomoFrameIdx f)
  let (PoniExt p _) = difTomoFramePoniExt f
  lift $ saveScript (poniToText p) filename
  yield $ DifTomoFrame' { difTomoFrame'DifTomoFrame = f
                        , difTomoFrame'PoniPath = filename
                        }

data DifTomoFrame'' sh = DifTomoFrame'' { difTomoFrame''DifTomoFrame' :: DifTomoFrame' sh
                                        , difTomoFrame''PySCript :: Text
                                        , difTomoFrame''PySCriptPath :: FilePath
                                        , difTomoFrame''DataPath :: FilePath
                                        }

savePy :: (Shape sh) => DIM1 -> Threshold -> Pipe (DifTomoFrame' sh) (DifTomoFrame'' sh) IO ()
savePy b t = forever $ do
  f@(DifTomoFrame' _difTomoFrame poniPath) <- await
  let directory = takeDirectory poniPath
  let scriptPath = dropExtension poniPath ++ ".py"
  let (script, dataPath) = createPy b t f
  lift $ saveScript script scriptPath
  ExitSuccess <- lift $ system (unwords ["cd ", directory, "&&",  "python", scriptPath])
  yield $ DifTomoFrame'' { difTomoFrame''DifTomoFrame' = f
                         , difTomoFrame''PySCript = script
                         , difTomoFrame''PySCriptPath = scriptPath
                         , difTomoFrame''DataPath = dataPath
                         }

saveGnuplot' :: (Shape sh) => Consumer (DifTomoFrame'' sh) (StateT [FilePath] IO) r
saveGnuplot' = forever $ do
  curves <- lift get
  (DifTomoFrame'' (DifTomoFrame' _ poniPath) _ _ dataPath) <- await
  let directory = takeDirectory poniPath
  let filename = directory </> "plot.gnuplot"
  lift . lift $ saveScript (new_content curves) filename
  lift $ put $! (curves ++ [dataPath])
    where
      new_content :: [FilePath] -> Text
      new_content cs = Text.unlines (lines' cs)

      lines' :: [FilePath] -> [Text]
      lines' cs = ["plot \\"]
                 ++ [Text.intercalate ",\\\n" [ Text.pack (show (takeFileName c) ++ " u 1:2 w l") | c <- cs ]]
                 ++ ["pause -1"]

saveGnuplot :: (Shape sh) => Consumer (DifTomoFrame'' sh) IO r
saveGnuplot = evalStateP [] saveGnuplot'

-- | PyFAI MultiGeometry

integrateMulti :: PoniExt -> XRDSample -> IO ()
integrateMulti ref (XRDSample _ output nxss) =
  mapM_ (integrateMulti' ref output) nxss

integrateMulti' :: PoniExt -> OutputBaseDir -> XrdNxs -> IO ()
integrateMulti' ref output (XrdNxs _ mb t (XrdSourceNxs nxs'@(Nxs f _ _))) = do
  print f
  withH5File f $ \h5file ->
      runSafeT $ runEffect $
        withDataFrameH5 h5file nxs' (gen ref) yield
        >-> hoist lift (frames
                        >-> savePonies (pgen output f)
                        >-> saveMultiGeometry mb t)
  where
    gen :: PoniExt -> Pose -> Int -> IO PoniExt
    gen ref' m _idx = return $ setPose ref' m

    pgen :: OutputBaseDir -> FilePath -> Int -> FilePath
    pgen o nxs'' idx = o </> scandir </>  scandir ++ printf "_%02d.poni" idx
      where
        scandir = (dropExtension . takeFileName) nxs''
integrateMulti' ref output (XrdNxs b _ t (XrdSourceEdf fs)) = do
  -- generate all the ponies
  zipWithM_ go fs ponies

  -- generate the multi.py python script
  let script = createMultiPyEdf b t fs ponies (output </> "multi.dat")
  saveScript script (output </> "multi.py")
    where
      ponies = [(output </> (dropExtension . takeFileName) f ++ ".poni") | f <- fs]

      go :: FilePath -> FilePath -> IO ()
      go f o = do
        m <- getMEdf f
        let (PoniExt p _) = setPose ref m
        saveScript (poniToText p) o

createMultiPy :: (Shape sh) => DIM1 -> Threshold -> (DifTomoFrame' sh) -> [FilePath] -> (Text, FilePath)
createMultiPy b (Threshold t) (DifTomoFrame' f _) ponies = (script, output)
    where
      script = Text.unlines $
               map Text.pack ["#!/bin/env python"
                             , ""
                             , "import numpy"
                             , "from h5py import File"
                             , "from pyFAI.multi_geometry import MultiGeometry"
                             , ""
                             , "NEXUSFILE = " ++ show nxs'
                             , "IMAGEPATH = " ++ show i'
                             , "BINS = " ++ show (size b)
                             , "OUTPUT = " ++ show output
                             , "WAVELENGTH = " ++ show (w /~ meter)
                             , "THRESHOLD = " ++ show t
                             , ""
                             , "# Load all images"
                             , "PONIES = [" ++ (List.intercalate ",\n" (map show ponies)) ++ "]"
                             , ""
                             , "# Read all the images"
                             , "with File(NEXUSFILE, mode='r') as f:"
                             , "    imgs = f[IMAGEPATH][:]"
                             , ""
                             , "# Compute the mask"
                             , "mask = numpy.zeros_like(imgs[0], dtype=bool)"
                             , "for img in imgs:"
                             , "    mask_t = numpy.where(img > THRESHOLD, True, False)"
                             , "    mask = numpy.logical_or(mask, mask_t)"
                             , ""
                             , "# Integration multi-geometry 1D"
                             , "mg = MultiGeometry(PONIES, unit=\"2th_deg\", radial_range=(0,80))"
                             , "p = mg.integrate1d(imgs, BINS, lst_mask=mask)"
                             , ""
                             , "# Save the datas"
                             , "numpy.savetxt(OUTPUT, numpy.array(p).T)"
                             ]
      (Nxs nxs' _ h5path') = difTomoFrameNxs f
      (DataItem i' _) = h5pImage h5path'
      output = "multi.dat"
      (Geometry _ (Source w) _ _) = difTomoFrameGeometry f

createMultiPyEdf :: DIM1 -> Threshold -> [FilePath] -> [FilePath] -> FilePath -> Text
createMultiPyEdf b (Threshold t) edfs ponies output = script
    where
      script = Text.unlines $
               map Text.pack ["#!/bin/env python"
                             , ""
                             , "import numpy"
                             , "from fabio import open"
                             , "from pyFAI.multi_geometry import MultiGeometry"
                             , ""
                             , "EDFS = [" ++ (List.intercalate ",\n" (map show edfs)) ++ "]"
                             , "PONIES = [" ++ (List.intercalate ",\n" (map show ponies)) ++ "]"
                             , "BINS = " ++ show (size b)
                             , "OUTPUT = " ++ show output
                             , "THRESHOLD = " ++ show t
                             , ""
                             , "# Read all the images"
                             , "imgs = [open(edf).data for edf in EDFS]"
                             , ""
                             , "# Compute the mask"
                             , "mask = numpy.zeros_like(imgs[0], dtype=bool)"
                             , "for img in imgs:"
                             , "    mask_t = numpy.where(img > THRESHOLD, True, False)"
                             , "    mask = numpy.logical_or(mask, mask_t)"
                             , ""
                             , "# Integration multi-geometry 1D"
                             , "mg = MultiGeometry(PONIES, unit=\"2th_deg\", radial_range=(0,80))"
                             , "p = mg.integrate1d(imgs, BINS, lst_mask=mask)"
                             , ""
                             , "# Save the datas"
                             , "numpy.savetxt(OUTPUT, numpy.array(p).T)"
                             ]

saveMulti' :: (Shape sh) => DIM1 -> Threshold -> Consumer (DifTomoFrame' sh) (StateT [FilePath] IO) r
saveMulti' b t = forever $ do
  ponies <- lift get
  f'@(DifTomoFrame' f poniPath) <- await
  let directory = takeDirectory poniPath
  let filename = directory </> "multi.py"
  let (script, _) = createMultiPy b t f' ponies
  lift . lift $ saveScript script filename
  lift . lift $ go directory filename (difTomoFrameEOF f)
  lift $ put $! (ponies ++ [poniPath])
      where
        go :: FilePath -> FilePath -> Bool -> IO ()
        go d s True = do
          ExitSuccess <- system (unwords ["cd ", d, "&&",  "python", s])
          return ()
        go _ _ False = return ()

saveMultiGeometry :: (Shape sh) => DIM1 -> Threshold -> Consumer (DifTomoFrame' sh) IO r
saveMultiGeometry b t = evalStateP [] (saveMulti' b t)

-- | XRD Mesh

-- data DataFrameMeshH5Path =
--     DataFrameMeshH5Path { dataFrameMeshH5Path'Image :: DataItem
--                         , dataFrameMeshH5Path'MeshX :: DataItem
--                         , dataFrameMeshH5Path'MeshY :: DataItem
--                         , dataFrameMeshH5Path'Gamma :: DataItem
--                         , dataFrameMeshH5Path'Delta :: DataItem
--                         , dataFrameMeshH5Path'Wavelength :: DataItem
--                         } deriving (Show)

-- data DataFrameH5' a =
--     DataFrameH5' { dataFrameH5'Nxs' :: Nxs' a
--                  , dataFrameH5'MeshX :: Dataset
--                  , dataFrameH5'MeshY :: Dataset
--                  , dataFrameH5'Gamma :: Dataset
--                  , dataFrameH5'Delta :: Dataset
--                  , dataFrameH5'Wavelength :: Dataset
--                  , dataFrameH5'Ponigen :: PoniGenerator
--                  }

-- data XRDSample' a = XRDSample' SampleName OutputBaseDir [XrdNxs' a] -- ^ nxss

-- data XrdNxs' a = XrdNxs' DIM1 DIM1 Threshold (Nxs' a) deriving (Show)

-- data Nxs' a = Nxs' FilePath NxEntry a deriving (Show)

-- class DataSource t where
--     withDataSource :: (MonadSafe m) => File -> Nxs' t -> PoniGenerator -> (DataFrameH5' t -> m r) -> m r

-- data DifTomoFrameND sh =
--     DifTomoFrame2D { difTomoFrameND'Nxs :: Nxs -- ^ nexus of the current frame
--                    , difTomoFrameND'Idxs :: sh -- ^ current index in the array
--                    , difTomoFrameND'EOF :: Bool -- ^ is it the eof of the stream
--                    , difTomoFrameND'Geometry :: Geometry -- ^ diffractometer geometry
--                    , difTomoFrameND'PoniExt :: PoniExt -- ^ the ref poniext
--                    } deriving (Show)

-- class FrameND t where
--   lenND :: t -> IO (Maybe Int)
--   rowND :: Shape sh => t -> sh -> IO (DifTomoFrameND sh)

-- framesND :: (Frame a, Shape sh) => Pipe a (DifTomoFrameND sh) IO ()
-- framesND = do
--   d <- await
--   (Just n) <- lift $ lenND d
--   forM_ [0..n-1] (\i' -> do
--                      f <- lift $ rowND d idx
--                      yield f)

-- instance DataSource DataFrameMeshH5Path where
--     withDataSource h nxs'@(Nxs' _ _ d) gen = bracket (liftIO before) (liftIO . after)
--         where
--           -- before :: File -> DataFrameH5Path -> m DataFrameH5
--           before :: IO (DataFrameH5' DataFrameMeshH5Path)
--           before =  DataFrameH5'
--                     <$> return nxs'
--                     <*> openDataset' h (dataFrameMeshH5Path'MeshX d)
--                     <*> openDataset' h (dataFrameMeshH5Path'MeshY d)
--                     <*> openDataset' h (dataFrameMeshH5Path'Gamma d)
--                     <*> openDataset' h (dataFrameMeshH5Path'Delta d)
--                     <*> openDataset' h (dataFrameMeshH5Path'Wavelength d)
--                     <*> return gen

--           after :: DataFrameH5' DataFrameMeshH5Path -> IO ()
--           after d' = do
--             closeDataset (dataFrameH5'MeshX d')
--             closeDataset (dataFrameH5'MeshY d')
--             closeDataset (dataFrameH5'Gamma d')
--             closeDataset (dataFrameH5'Delta d')
--             closeDataset (dataFrameH5'Wavelength d')

--           openDataset' :: File -> DataItem -> IO Dataset
--           openDataset' hid (DataItem name _) = openDataset hid (Char8.pack name) Nothing


-- integrateMesh :: (DataSource a) => PoniExt -> XRDSample' a -> IO ()
-- integrateMesh ref (XRDSample' _ output nxss) =
--   mapM_ (integrateMesh' ref output) nxss

-- integrateMesh' :: (DataSource a) => PoniExt -> OutputBaseDir -> XrdNxs' a -> IO ()
-- integrateMesh' ref output (XrdNxs' _ mb t nxs'@(Nxs' f _ _)) = do
--   print f
--   withH5File f $ \h5file ->
--       runSafeT $ runEffect $
--         withDataSource h5file nxs' (gen ref) yield
--         >-> hoist lift (framesND)
--         --                 >-> savePonies (pgen output f)
--         --                 >-> saveMultiGeometry mb t)
--   where
--     gen :: PoniExt -> Pose -> Int -> IO PoniExt
--     gen ref' m _idx = return $ setPose ref' m

--     pgen :: OutputBaseDir -> FilePath -> Int -> FilePath
--     pgen o nxs'' idx = o </> scandir </>  scandir ++ printf "_%02d.poni" idx
--       where
--         scandir = (dropExtension . takeFileName) nxs''
