{-# LANGUAGE CPP #-}
module Hkl.Diffabs
    ( main_diffabs )
    where

import Prelude hiding (concat, head, print)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Vector.Storable (concat, head)
import Numeric.Units.Dimensional.Prelude (meter, nano, (*~))
import Pipes (Producer, lift, (>->), runEffect, yield)
import Pipes.Prelude (print)
import System.FilePath.Posix ((</>))

import Hkl.Types
import Hkl.H5

{-# ANN module "HLint: ignore Use camelCase" #-}

data DataFrameH5Path =
  DataFrameH5Path { h5pImage :: DataItem
                  , h5pMu :: DataItem
                  , h5pKomega :: DataItem
                  , h5pKappa :: DataItem
                  , h5pKphi :: DataItem
                  , h5pGamma :: DataItem
                  , h5pDelta :: DataItem
                  , h5pWavelength :: DataItem
                  , h5pDiffractometerType :: DataItem
                  } deriving (Show)

data DataFrameH5 =
  DataFrameH5 { h5image :: Dataset
              , h5mu :: Dataset
              , h5komega :: Dataset
              , h5kappa :: Dataset
              , h5kphi :: Dataset
              , h5gamma :: Dataset
              , h5delta :: Dataset
              , h5wavelength :: Dataset
              , h5dtype :: Dataset
              }

data DataFrame =
  DataFrame { df_n :: Int
            , df_geometry :: Geometry
            } deriving (Show)

withDataframeH5 :: File -> DataFrameH5Path -> (DataFrameH5 -> IO r) -> IO r
withDataframeH5 h5file dfp = bracket (hkl_h5_open h5file dfp) hkl_h5_close

hkl_h5_open :: File -> DataFrameH5Path -> IO DataFrameH5
hkl_h5_open h5file dp = DataFrameH5
                         <$> openDataset' h5file (h5pImage dp)
                         <*> openDataset' h5file (h5pMu dp)
                         <*> openDataset' h5file (h5pKomega dp)
                         <*> openDataset' h5file (h5pKappa dp)
                         <*> openDataset' h5file (h5pKphi dp)
                         <*> openDataset' h5file (h5pGamma dp)
                         <*> openDataset' h5file (h5pDelta dp)
                         <*> openDataset' h5file (h5pWavelength dp)
                         <*> openDataset' h5file (h5pDiffractometerType dp)
  where
    openDataset' hid (DataItem name _) = openDataset hid (pack name) Nothing

hkl_h5_is_valid :: DataFrameH5-> IO Bool
hkl_h5_is_valid d = do
  True <- check_ndims (h5mu d) 1
  True <- check_ndims (h5komega d) 1
  True <- check_ndims (h5kappa d) 1
  True <- check_ndims (h5kphi d) 1
  True <- check_ndims (h5gamma d) 1
  True <- check_ndims (h5delta d) 1
  return True

hkl_h5_close :: DataFrameH5 -> IO ()
hkl_h5_close df = do
  closeDataset (h5image df)
  closeDataset (h5mu df)
  closeDataset (h5komega df)
  closeDataset (h5kappa df)
  closeDataset (h5kphi df)
  closeDataset (h5gamma df)
  closeDataset (h5delta df)
  closeDataset (h5wavelength df)
  closeDataset (h5dtype df)

getDataFrame' ::  DataFrameH5 -> Int -> IO DataFrame
getDataFrame' d i = do
  mu <- get_position (h5mu d) i
  komega <- get_position (h5komega d) i
  kappa <- get_position (h5kappa d) i
  kphi <- get_position (h5kphi d) i
  gamma <- get_position (h5gamma d) i
  delta <- get_position (h5delta d) i
  wavelength <- get_position (h5wavelength d) 0
  let source = Source (head wavelength *~ nano meter)
  let positions = concat [mu, komega, kappa, kphi, gamma, delta]
  return DataFrame { df_n = i
                   , df_geometry = Geometry source positions Nothing
                   }

getDataFrame :: DataFrameH5 -> Producer DataFrame IO ()
getDataFrame d = do
  (Just n) <- lift $ lenH5Dataspace (h5delta d)
  forM_ [0..n-1] (\i -> lift (getDataFrame' d i) >>= yield)

main_diffabs :: IO ()
main_diffabs = do
  let root = "/tmp"
  let filename = "XRD18keV_27.nxs"
  let dataframe_h5p = DataFrameH5Path
                      { h5pImage = DataItem "scan_27/scan_data/data_53" StrictDims
                      , h5pMu = DataItem "scan_27/DIFFABS/d13-1-cx1__EX__DIF.1-MU__#1/raw_value" ExtendDims
                      , h5pKomega = DataItem "scan_27/DIFFABS/d13-1-cx1__EX__DIF.1-KOMEGA__#1/raw_value" ExtendDims
                      , h5pKappa = DataItem "scan_27/DIFFABS/d13-1-cx1__EX__DIF.1-KAPPA__#1/raw_value" ExtendDims
                      , h5pKphi = DataItem "scan_27/DIFFABS/d13-1-cx1__EX__DIF.1-KPHI__#1/raw_value" ExtendDims
                      , h5pGamma = DataItem "scan_27/DIFFABS/d13-1-cx1__EX__DIF.1-GAMMA__#1/raw_value" ExtendDims
                      , h5pDelta = DataItem "scan_27/scan_data/trajectory_1_1" ExtendDims
                      , h5pWavelength = DataItem "scan_27/DIFFABS/D13-1-C03__OP__MONO__#1/wavelength" StrictDims
                      , h5pDiffractometerType = DataItem "scan_27/DIFFABS/I14-C-CX2__EX__DIFF-UHV__#1/type" StrictDims
                      }

  withH5File (root </> filename) $ \h5file ->
    withDataframeH5 h5file dataframe_h5p $ \dataframe_h5 -> do
      True <- hkl_h5_is_valid dataframe_h5

      runEffect $ getDataFrame dataframe_h5
        >-> print
