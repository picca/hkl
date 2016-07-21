{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Hkl.Diffabs.Martinetto
       ( main_martinetto ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Attoparsec.Text
import Data.ByteString.Char8 (pack)
import Data.Either
import Data.List (sort)
import Data.Packed.Matrix (Matrix)
import Data.Text (Text, intercalate, pack)
import Data.Text.IO
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
import System.FilePath ((</>))
import System.FilePath.Glob
import System.IO (withFile, IOMode(WriteMode) )

import Prelude hiding (concat, head, lookup, print, readFile)

import Pipes (Producer, lift, (>->), runEffect, yield)
import Pipes.Prelude (print)


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

title :: Text
title = intercalate (Data.Text.pack "\t")
        (map Data.Text.pack [ "# distance"
                            , "poni1"
                            , "poni2"
                            , "rot1"
                            , "rot2"
                            , "rot3" ])

toText :: PoniEntry -> Text
toText (PoniEntry _ _ _ _ d p1 p2 rot1 rot2 rot3 _ _) =
  intercalate (Data.Text.pack "\t")
  (map (Data.Text.pack . show) [ d /~ meter
                               , p1 /~ meter
                               , p2 /~ meter
                               , rot1 /~ degree
                               , rot2 /~ degree
                               , rot3 /~ degree])



save :: FilePath -> [PoniEntry] -> IO ()
save f ps = do
  withFile f WriteMode $ \handler -> do
    hPutStrLn handler title
    mapM_ (put handler) ps
    where
      put h p = hPutStrLn h (toText p)

ponies :: [FilePath] -> IO [PoniEntry]
ponies fs = mapM extract (sort fs)
  where
     extract :: FilePath -> IO PoniEntry
     extract filename = do
       content <- readFile filename
       return $ case (parseOnly poniP content) of
         Left _ -> error $ "Can not parse the " ++ filename ++ " poni file"
         Right poni -> last poni

data DataFrame =
  DataFrame { df_n :: Int -- ^ index of the current frame
            , df_geometry :: Geometry -- ^ diffractometer geometry
            , df_m :: Matrix Double -- ^ the detector orientation matrix
            } deriving (Show)

class Frame t where
  len :: t -> IO (Maybe Int)
  row :: t -> Int -> IO DataFrame

data DataFrameH5Path =
  DataFrameH5Path { h5pImage :: DataItem
                  , h5pGamma :: DataItem
                  , h5pDelta :: DataItem
                  , h5pWavelength :: DataItem
                  } deriving (Show)

data DataFrameH5 =
  DataFrameH5 { h5image :: Dataset
              , h5gamma :: Dataset
              , h5delta :: Dataset
              , h5wavelength :: Dataset
              }

instance Frame DataFrameH5 where
  len d =  lenH5Dataspace (h5delta d)

  row d idx = do
    let mu = 0.0
    let komega = 0.0
    let kappa = 0.0
    let kphi = 0.0
    gamma <- get_position (h5gamma d) 0
    delta <- get_position (h5delta d) idx
    wavelength <- get_position (h5wavelength d) 0
    let source = Source (head wavelength *~ nano meter)
    let positions = concat [mu, komega, kappa, kphi, gamma, delta]
    let geometry =  Geometry source positions Nothing
    let detector = Detector DetectorType0D
    m <- geometryDetectorRotationGet K6c geometry detector
    return DataFrame { df_n = idx
                     , df_geometry = geometry
                     , df_m = m
                     }

withDataframeH5 :: File -> DataFrameH5Path -> (DataFrameH5 -> IO r) -> IO r
withDataframeH5 h5file dfp = bracket (acquire h5file dfp) release
  where
    acquire :: File -> DataFrameH5Path -> IO DataFrameH5
    acquire h d =  DataFrameH5
                   <$> openDataset' h (h5pImage d)
                   <*> openDataset' h (h5pGamma d)
                   <*> openDataset' h (h5pDelta d)
                   <*> openDataset' h (h5pWavelength d)

    release :: DataFrameH5 -> IO ()
    release d = do
      closeDataset (h5image d)
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



frames :: Frame a => a -> Producer DataFrame IO ()
frames d = do
  (Just n) <- lift $ len d
  frames' d [0..n-1]

frames' :: Frame a => a -> [Int] -> Producer DataFrame IO ()
frames' d idxs = do
  forM_ idxs (\i -> lift (row d i) >>= yield)

main_martinetto :: IO ()
main_martinetto = do
  let project = "/nfs/ruche-diffabs/diffabs-users/99160066/"
  let published = project </> "published-data"
  let calibration = published </> "calibration"
  let output = calibration </> "ponies.txt"
  let nxs = calibration </> "XRD18keV_26.nxs"
  -- let filename = "/home/picca/tmp/reguer/rocha/merged.poni"
  -- let filename = "../cirpad/blender/test2.poni"
  filenames <- glob $ calibration </> "XRD18keV_26*.poni"
  -- print $ sort filenames
  -- print $ [0,3..39 :: Int] ++ [43 :: Int]
  -- print nxs
  entries <- ponies filenames
  save output entries

  -- lire le poni de référence ainsi que la géométrie associée.

  -- calculer et écrire pour chaque point d'un scan un poni correspondant à la bonne géométries.
  
  let nxentry = "scan_26"
  let beamline = "DIFFABS"
  let dataframe_h5p = DataFrameH5Path
                      { h5pImage = DataItem (nxentry </> "scan_data/data_53") StrictDims
                      , h5pGamma = DataItem (nxentry </> beamline </> "d13-1-cx1__EX__DIF.1-GAMMA__#1/raw_value") ExtendDims
                      , h5pDelta = DataItem (nxentry </> "scan_data/trajectory_1_1") ExtendDims
                      , h5pWavelength = DataItem (nxentry </> beamline </> "D13-1-C03__OP__MONO__#1/wavelength") StrictDims
                      }
  withH5File nxs $ \h5file ->
    withDataframeH5 h5file dataframe_h5p $ \dataframe_h5 -> do
      True <- hkl_h5_is_valid dataframe_h5

      runEffect $ frames dataframe_h5
        >-> print

  -- créer le script python d'intégration multi géométrie

  -- l'executer pour faire l'intégration.

  -- plot de la figure. (script python ou autre ?)
