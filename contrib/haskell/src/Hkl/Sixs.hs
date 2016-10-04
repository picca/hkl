{-# LANGUAGE CPP #-}
module Hkl.Sixs
       ( main_sixs )
       where

import Prelude hiding (concat, head, print)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

import Data.ByteString.Char8 (pack)
import Data.Vector.Storable (concat, head)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Numeric.LinearAlgebra (Matrix)
import Numeric.Units.Dimensional.Prelude (meter, nano, (*~))
import Pipes (Producer, runEffect, (>->), lift, yield)
import Pipes.Prelude (print)
import System.FilePath.Posix ((</>))

import Hkl.Types
import Hkl.H5

{-# ANN module "HLint: ignore Use camelCase" #-}

data DataFrameH5Path = DataFrameH5Path
                       { h5pImage :: DataItem
                       , h5pMu :: DataItem
                       , h5pOmega :: DataItem
                       , h5pDelta :: DataItem
                       , h5pGamma :: DataItem
                       , h5pUB :: DataItem
                       , h5pWavelength :: DataItem
                       , h5pDiffractometerType :: DataItem
                       } deriving (Show)

data DataFrameH5 = DataFrameH5
                   { h5image :: Dataset
                   , h5mu :: Dataset
                   , h5omega :: Dataset
                   , h5delta :: Dataset
                   , h5gamma :: Dataset
                   , h5ub :: Dataset
                   , h5wavelength :: Dataset
                   , h5dtype :: Dataset
                   }

data DataFrame = DataFrame
                 { df_n :: Int
                 , df_geometry :: Geometry
                 , df_ub :: Matrix Double
                 } deriving (Show)

withDataframeH5 :: File -> DataFrameH5Path -> (DataFrameH5 -> IO r) -> IO r
withDataframeH5 h5file dfp = bracket (hkl_h5_open h5file dfp) hkl_h5_close

hkl_h5_open :: File -> DataFrameH5Path -> IO DataFrameH5
hkl_h5_open h5file dp = DataFrameH5
                         <$> openDataset' h5file (h5pImage dp)
                         <*> openDataset' h5file (h5pMu dp)
                         <*> openDataset' h5file (h5pOmega dp)
                         <*> openDataset' h5file (h5pDelta dp)
                         <*> openDataset' h5file (h5pGamma dp)
                         <*> openDataset' h5file (h5pUB dp)
                         <*> openDataset' h5file (h5pWavelength dp)
                         <*> openDataset' h5file (h5pDiffractometerType dp)
  where
    openDataset' hid (DataItem name _) = openDataset hid (pack name) Nothing

hkl_h5_is_valid :: DataFrameH5 -> IO Bool
hkl_h5_is_valid df = do
  True <- check_ndims (h5mu df) 1
  True <- check_ndims (h5omega df) 1
  True <- check_ndims (h5delta df) 1
  True <- check_ndims (h5gamma df) 1
  return True

hkl_h5_close :: DataFrameH5 -> IO ()
hkl_h5_close d = do
  closeDataset (h5image d)
  closeDataset (h5mu d)
  closeDataset (h5omega d)
  closeDataset (h5delta d)
  closeDataset (h5gamma d)
  closeDataset (h5ub d)
  closeDataset (h5wavelength d)
  closeDataset (h5dtype d)

getDataFrame' ::  DataFrameH5 -> Int -> IO DataFrame
getDataFrame' d i = do
  mu <- get_position (h5mu d) i
  omega <- get_position (h5omega d) i
  delta <- get_position (h5delta d) i
  gamma <- get_position (h5gamma d) i
  wavelength <- get_position (h5wavelength d) 0
  ub <- get_ub (h5ub d)
  let positions = concat [mu, omega, delta, gamma]
  let source = Source (head wavelength *~ nano meter)
  return DataFrame { df_n = i
                   , df_geometry = Geometry Uhv source positions Nothing
                   , df_ub = ub
                   }

getDataFrame :: DataFrameH5 -> Producer DataFrame IO ()
getDataFrame d = do
  (Just n) <- lift $ lenH5Dataspace (h5mu d)
  forM_ [0..n-1] (\i -> lift (getDataFrame' d i) >>= yield)

main_sixs :: IO ()
main_sixs = do
  let root = "/nfs/ruche-sixs/sixs-soleil/com-sixs/2015/Shutdown4-5/XpadAu111/"
  let filename = "align_FLY2_omega_00045.nxs"
  let dataframe_h5p = DataFrameH5Path { h5pImage = DataItem "com_113934/scan_data/xpad_image" StrictDims
                                      , h5pMu = DataItem "com_113934/scan_data/UHV_MU" ExtendDims
                                      , h5pOmega = DataItem "com_113934/scan_data/UHV_OMEGA" ExtendDims
                                      , h5pDelta = DataItem "com_113934/scan_data/UHV_DELTA" ExtendDims
                                      , h5pGamma = DataItem "com_113934/scan_data/UHV_GAMMA" ExtendDims
                                      , h5pUB = DataItem "com_113934/SIXS/I14-C-CX2__EX__DIFF-UHV__#1/UB" StrictDims
                                      , h5pWavelength = DataItem "com_113934/SIXS/Monochromator/wavelength" StrictDims
                                      , h5pDiffractometerType = DataItem "com_113934/SIXS/I14-C-CX2__EX__DIFF-UHV__#1/type" StrictDims
                                      }

  withH5File (root </> filename) $ \h5file ->
    withDataframeH5 h5file dataframe_h5p $ \dataframe_h5 -> do
      True <- hkl_h5_is_valid dataframe_h5
      runEffect $ getDataFrame dataframe_h5
        >->  print
