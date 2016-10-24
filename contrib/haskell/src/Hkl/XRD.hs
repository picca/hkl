{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

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
         -- reference
       , getPoniExtRef
         -- integration
       , integrate
       ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.State.Strict (StateT, get, put)
import Data.Attoparsec.Text (parseOnly)
import qualified Data.ByteString.Char8 as Char8 (pack)
import Data.Text (Text)
import qualified Data.Text as Text (unlines, pack, intercalate)
import Data.Text.IO (readFile, writeFile)
import Data.Vector.Storable (concat, head)
import Numeric.Units.Dimensional.Prelude (meter, nano, (/~), (*~))
import System.Directory (createDirectoryIfMissing)
import System.Exit ( ExitCode( ExitSuccess ) )
import System.FilePath ((</>), dropExtension, takeFileName, takeDirectory)
import System.Process ( system )
import Text.Printf ( printf )

import Prelude hiding
    ( concat
    , head
    , lookup
    , readFile
    , writeFile
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
import Hkl.H5
import Hkl.PyFAI
import Hkl.MyMatrix
import Hkl.PyFAI.PoniExt
import Hkl.Types

-- | Types

type NxEntry = String
type OutputBaseDir = FilePath
type PoniGenerator = Pose -> Int -> IO PoniExt
type SampleName = String

data Bins = Bins Int
          deriving (Show)

data Threshold = Threshold Int
               deriving (Show)

data XRDRef = XRDRef SampleName OutputBaseDir Nxs Int

data XRDSample = XRDSample SampleName OutputBaseDir [XrdNxs]-- ^ nxss

data XrdNxs = XrdNxs Bins Threshold Nxs deriving (Show)

data Nxs = Nxs FilePath NxEntry DataFrameH5Path deriving (Show)

data DifTomoFrame =
  DifTomoFrame { difTomoFrameNxs :: Nxs -- ^ nexus of the current frame
               , difTomoFrameIdx :: Int -- ^ index of the current frame
               , difTomoFrameGeometry :: Geometry -- ^ diffractometer geometry
               , difTomoFramePoniExt :: PoniExt -- ^ the ref poniext
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
    let source = Source (head wavelength *~ nano meter)
    let positions = concat [mu, komega, kappa, kphi, gamma, delta]
    let geometry =  Geometry K6c source positions Nothing
    let detector = ZeroD
    m <- geometryDetectorRotationGet geometry detector
    poniext <- ponigen d (MyMatrix HklB m) idx
    return DifTomoFrame { difTomoFrameNxs = nxs'
                        , difTomoFrameIdx = idx
                        , difTomoFrameGeometry = geometry
                        , difTomoFramePoniExt = poniext
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
  return $ difTomoFramePoniExt (Prelude.last poniExtRefs)
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

createPy :: Bins -> Threshold -> DifTomoFrame' -> (Text, FilePath)
createPy (Bins b) (Threshold t) (DifTomoFrame' f poniPath) = (script, output)
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
                             , "N = " ++ show b
                             , "OUTPUT = " ++ show output
                             , "WAVELENGTH = " ++ show (w /~ meter)
                             , "THRESHOLD = " ++ show t
                             , ""
                             , "ai = load(PONIFILE)"
                             , "ai.wavelength = WAVELENGTH"
                             , "ai._empty = numpy.nan"
                             , "mask_det = ai.detector.mask"
                             , "#mask_module = numpy.zeros_like(mask_det)"
                             , "#mask_module[0:120, :] = True"
                             , "with File(NEXUSFILE, mode='r') as f:"
                             , "    img = f[IMAGEPATH][IDX]"
                             , "    mask = numpy.where(img > THRESHOLD, True, False)"
                             , "    mask = numpy.logical_or(mask, mask_det)"
                             , "    #mask = numpy.logical_or(mask, mask_module)"
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

data DifTomoFrame' = DifTomoFrame' { difTomoFrame'DifTomoFrame :: DifTomoFrame
                                   , difTomoFrame'PoniPath :: FilePath
                                   }

savePonies :: (Int -> FilePath) -> Pipe DifTomoFrame DifTomoFrame' IO ()
savePonies g = forever $ do
  f <- await
  let filename = g (difTomoFrameIdx f)
  lift $ createDirectoryIfMissing True (takeDirectory filename)
  lift $ writeFile filename (content (difTomoFramePoniExt f))
  lift $ print $ "--> " ++ filename
  yield $ DifTomoFrame' { difTomoFrame'DifTomoFrame = f
                        , difTomoFrame'PoniPath = filename
                        }
    where
      content :: PoniExt -> Text
      content (PoniExt poni _) = poniToText poni

data DifTomoFrame'' = DifTomoFrame'' { difTomoFrame''DifTomoFrame' :: DifTomoFrame'
                                     , difTomoFrame''PySCript :: Text
                                     , difTomoFrame''PySCriptPath :: FilePath
                                     , difTomoFrame''DataPath :: FilePath
                                     }

savePy :: Bins -> Threshold -> Pipe DifTomoFrame' DifTomoFrame'' IO ()
savePy b t = forever $ do
  f@(DifTomoFrame' _difTomoFrame poniPath) <- await
  let directory = takeDirectory poniPath
  let scriptPath = dropExtension poniPath ++ ".py"
  let (script, dataPath) = createPy b t f
  lift $ createDirectoryIfMissing True directory
  lift $ writeFile scriptPath script
  lift $ print $ "--> " ++ scriptPath
  ExitSuccess <- lift $ system (unwords ["cd ", directory, "&&",  "python", scriptPath])
  yield $ DifTomoFrame'' { difTomoFrame''DifTomoFrame' = f
                         , difTomoFrame''PySCript = script
                         , difTomoFrame''PySCriptPath = scriptPath
                         , difTomoFrame''DataPath = dataPath
                         }

saveGnuplot' :: Consumer DifTomoFrame'' (StateT [FilePath] IO) r
saveGnuplot' = forever $ do
  curves <- lift get
  (DifTomoFrame'' (DifTomoFrame' _ poniPath) _ _ dataPath) <- await
  let directory = takeDirectory poniPath
  let filename = directory </> "plot.gnuplot"
  lift . lift $ print $ "gnuplot --> " ++ filename
  lift . lift $ createDirectoryIfMissing True directory
  lift . lift $ writeFile filename (new_content curves)
  lift $ put $! (curves ++ [dataPath])
    where
      new_content :: [FilePath] -> Text
      new_content cs = Text.unlines (lines' cs)

      lines' :: [FilePath] -> [Text]
      lines' cs = ["plot \\"]
                 ++ [Text.intercalate ",\\\n" [ Text.pack (show (takeFileName c) ++ " u 1:2 w l") | c <- cs ]]
                 ++ ["pause -1"]

saveGnuplot :: Consumer DifTomoFrame'' IO r
saveGnuplot = evalStateP [] saveGnuplot'

-- createGnuplot :: FilePath -> Text
-- createGnuplot f = Data.Text.unline $
--                   map Data.Text.pack ["plot for [i=0:" ++ n ++ "] sprintf(\"" ++ N27T2_14_ ++ "%02d.dat\", i) u 1:2 w l"
--                                      , "pause -1"
--                                      ]
--                       where
--                         n =

frames :: (Frame a) => Pipe a DifTomoFrame IO ()
frames = do
  d <- await
  (Just n) <- lift $ len d
  forM_ [0..n-1] (\i' -> do
                     f <- lift $ row d i'
                     yield f)

frames' :: (Frame a) => [Int] -> Pipe a DifTomoFrame IO ()
frames' is = do
  d <- await
  forM_ is (\i' -> do
              f <- lift $ row d i'
              yield f)
