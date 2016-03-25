module Hkl.Diffabs
    ( main_diffabs )
    where

import Bindings.HDF5.Raw
import Control.Applicative
import Control.Monad (forever, forM_)
import Foreign.C.String
import Foreign.Ptr.Conventions
import Hkl.H5
import Pipes
import Pipes.Prelude
import System.FilePath.Posix

{-# ANN module "HLint: ignore Use camelCase" #-}

data DataFrameH5Path =
  DataFrameH5Path { h5pImage :: String
                  , h5pMu :: String
                  , h5pKomega :: String
                  , h5pKappa :: String
                  , h5pKphi :: String
                  , h5pGamma :: String
                  , h5pDelta :: String
                  , h5pWaveLength :: String
                  , h5pDiffractometerType :: String
                  } deriving (Show)

data DataFrameH5 =
  DataFrameH5 { h5file :: HId_t
              , h5image :: HId_t
              , h5mu :: HId_t
              , h5komega :: HId_t
              , h5kappa :: HId_t
              , h5kphi :: HId_t
              , h5delta :: HId_t
              , h5gamma :: HId_t
              , h5wavelength :: HId_t
              , h5dtype :: HId_t
              } deriving (Show)

data DataFrame =
  DataFrame { df_n :: Int
            , df_image :: [Double]
            } deriving (Show)

-- static herr_t attribute_info(hid_t location_id, const char *attr_name, const H5A_info_t *ainfo, void *op_data)
-- {
-- 	printf("    Attribute: %d %s\n", location_id, attr_name);

-- 	return 0;
-- }

-- static herr_t file_info(hid_t loc_id, const char *name, const H5L_info_t *info, void *opdata)
-- {
-- 	H5O_info_t statbuf;
-- 	hsize_t n = 0;

-- 	/*
-- 	 * Get type of the object and display its name and type.
-- 	 * The name of the object is passed to this function by
-- 	 * the Library. Some magic :-)
-- 	 */
-- 	H5Oget_info_by_name(loc_id, name, &statbuf, H5P_DEFAULT);
-- 	switch (statbuf.type) {
-- 	case H5O_TYPE_UNKNOWN:
-- 		printf(" Object with name %s is an unknown type\n", name);
-- 		break;
-- 	case H5O_TYPE_GROUP:
-- 		printf(" Object with name %s is a group\n", name);
-- 		break;
-- 	case H5O_TYPE_DATASET:
-- 		printf(" Object with name %s is a dataset\n", name);
-- 		break;
-- 	case H5O_TYPE_NAMED_DATATYPE:
-- 		printf(" Object with name %s is a named datatype\n", name);
-- 		break;
-- 	default:
-- 		printf(" Unable to identify an object ");
-- 	}

-- 	H5Aiterate_by_name(loc_id,  name, H5_INDEX_NAME, H5_ITER_NATIVE, &n, attribute_info, NULL, H5P_DEFAULT);

-- 	return 0;
-- }

hkl_h5_open :: FilePath -> DataFrameH5Path -> IO DataFrameH5
hkl_h5_open f dp = do
  file_id <- withCString f (\file -> h5f_open file h5f_ACC_RDONLY h5p_DEFAULT)
  DataFrameH5 file_id
    <$> get file_id h5pImage
    <*> get file_id h5pMu
    <*> get file_id h5pKomega
    <*> get file_id h5pKappa
    <*> get file_id h5pKphi
    <*> get file_id h5pGamma
    <*> get file_id h5pDelta
    <*> get file_id h5pWaveLength
    <*> get file_id h5pDiffractometerType
      where
        get fi a = withCString (a dp) (\dataset -> h5d_open2 fi dataset h5p_DEFAULT)

hkl_h5_is_valid :: DataFrameH5-> IO Bool
hkl_h5_is_valid d = do
  check_ndims (h5mu d) 1
  check_ndims (h5komega d) 1
  check_ndims (h5kappa d) 1
  check_ndims (h5kphi d) 1
  check_ndims (h5gamma d) 1
  check_ndims (h5delta d) 1

hkl_h5_close :: DataFrameH5 -> IO ()
hkl_h5_close d = do
  h5d_close (h5image d)
  h5d_close (h5mu d)
  h5d_close (h5komega d)
  h5d_close (h5kappa d)
  h5d_close (h5kphi d)
  h5d_close (h5gamma d)
  h5d_close (h5delta d)
  h5d_close (h5wavelength d)
  h5d_close (h5dtype d)
  h5f_close (h5file d)
  return ()

-- static herr_t hkl_dataframe_geometry_get(const HklDataframe dataframe, HklGeometry **geometry)
-- {
-- 	herr_t status = 0;
-- 	hid_t datatype;
-- 	double wavelength;
-- 	double axes[4];

-- 	/* create the HklGeometry */
-- 	if((*geometry) == NULL){
-- 		char *name;
-- 		size_t n;
-- 		HklFactory *factory;

-- 		/* read the diffractometer type from the hdf5 file */
-- 		datatype = H5Dget_type(dataframe._dataframe->dtype);
-- 		n = H5Tget_size(datatype);
-- 		name = malloc(n+1);
-- 		status = H5Dread(dataframe._dataframe->dtype,
-- 				 datatype,
-- 				 H5S_ALL, H5S_ALL,
-- 				 H5P_DEFAULT, name);
-- 		if(status >= 0){
-- 			/* remove the last "\n" char */
-- 			name[n-1] = 0;

-- 			factory = hkl_factory_get_by_name(name, NULL);
-- 			*geometry = hkl_factory_create_new_geometry(factory);
-- 		}
-- 		free(name);
-- 		H5Tclose(datatype);
-- 	}

-- 	/* read the wavelength double */
-- 	/* TODO check the right size */
-- 	/* TODO how to obtain the unit of the  wavelength */
-- 	datatype = H5Dget_type(dataframe._dataframe->wavelength);
-- 	status = H5Dread(dataframe._dataframe->wavelength,
-- 			 datatype,
-- 			 H5S_ALL, H5S_ALL,
-- 			 H5P_DEFAULT, &wavelength);
-- 	if(status >= 0)
-- 		hkl_geometry_wavelength_set(*geometry, wavelength, HKL_UNIT_USER, NULL);
-- 	H5Tclose(datatype);

-- 	/* read the axis positions of the ith dataframe */
-- 	/* check how to decide about the dataset connection and the hkl axes connection */
-- 	/* TODO check the right size */
-- 	/* TODO how to obtain the unit of the axes position */
-- 	if (get_position(dataframe._dataframe->mu,
-- 			 dataframe.i, &axes[0]) < 0)
-- 		goto out;
-- 	if (get_position(dataframe._dataframe->omega,
-- 			 dataframe.i, &axes[1]) < 0)
-- 		goto out;
-- 	if (get_position(dataframe._dataframe->gamma,
-- 			 dataframe.i, &axes[2]) < 0)
-- 		goto out;
-- 	if (get_position(dataframe._dataframe->delta,
-- 			 dataframe.i, &axes[3]) < 0)
-- 		goto out;

-- 	hkl_geometry_axis_values_set(*geometry, axes, 4, HKL_UNIT_USER, NULL);
-- 	/* hkl_geometry_fprintf(stdout, *geometry); */
-- 	/* fprintf(stdout, "\n"); */

-- 	return 0;
-- out:
-- 	return -1;

-- }

getDataFrame' ::  DataFrameH5 -> Int -> IO DataFrame
getDataFrame' d i = do
  mu <- get_position (h5mu d) i
  komega <- get_position (h5komega d) i
  kappa <- get_position (h5kappa d) i
  kphi <- get_position (h5kphi d) i
  gamma <- get_position (h5gamma d) i
  delta <- get_position (h5delta d) i
  return DataFrame { df_n = i
                   , df_image = mu ++ komega ++ kappa ++ kphi ++ gamma ++ delta
                   }

getDataFrame :: DataFrameH5 -> Producer DataFrame IO ()
getDataFrame d = do
  n <- lift $ hkl_h5_len (h5mu d)
  forM_ [0..n-1] (\i -> lift (getDataFrame' d i) >>= yield)

main_diffabs :: IO ()
main_diffabs = do
  let root = "/tmp"
  let filename = "XRD18keV_27.nxs"
  let dataframe_h5p = DataFrameH5Path { h5pImage = "scan_27/scan_data/data_53"
                                      , h5pMu = "scan_27/DIFFABS/d13-1-cx1__EX__DIF.1-MU__#1/raw_value"
                                      , h5pKomega = "scan_27/DIFFABS/d13-1-cx1__EX__DIF.1-KOMEGA__#1/raw_value"
                                      , h5pKappa = "scan_27/DIFFABS/d13-1-cx1__EX__DIF.1-KAPPA__#1/raw_value"
                                      , h5pKphi = "scan_27/DIFFABS/d13-1-cx1__EX__DIF.1-KPHI__#1/raw_value"
                                      , h5pGamma = "scan_27/DIFFABS/d13-1-cx1__EX__DIF.1-GAMMA__#1/raw_value"
                                      , h5pDelta = "scan_27/scan_data/trajectory_1_1"
                                      , h5pWaveLength = "scan_27/DIFFABS/D13-1-C03__OP__MONO__#1/wavelength"
                                      , h5pDiffractometerType = "scan_27/DIFFABS/I14-C-CX2__EX__DIFF-UHV__#1/type"
                                      }

  dataframe_h5 <- hkl_h5_open (root </> filename) dataframe_h5p

  hkl_h5_is_valid dataframe_h5

  runEffect $ getDataFrame dataframe_h5
            >-> Pipes.Prelude.print

  hkl_h5_close dataframe_h5
