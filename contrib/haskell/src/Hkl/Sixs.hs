module Hkl.Sixs
       ( main_sixs )
       where

import Bindings.HDF5.Raw
import Control.Applicative ((<*>), (<$>))
import Control.Exception (bracket)
import Control.Monad (forM_)
import Foreign.C.String (withCString)
import Hkl
import Numeric.Units.Dimensional.Prelude (meter, nano, (*~))
import Pipes (Producer, runEffect, (>->), lift, yield)
import Pipes.Prelude (print)
import System.FilePath.Posix ((</>))

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
                   { h5file :: HId_t
                   , h5image :: Maybe HId_t
                   , h5mu :: Maybe HId_t
                   , h5omega :: Maybe HId_t
                   , h5delta :: Maybe HId_t
                   , h5gamma :: Maybe HId_t
                   , h5ub :: Maybe HId_t
                   , h5wavelength :: Maybe HId_t
                   , h5dtype :: Maybe HId_t
                   } deriving (Show)

data DataFrame = DataFrame
                 { df_n :: Int
                 , df_geometry :: Geometry
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

withDataframeH5 :: HId_t -> DataFrameH5Path -> (DataFrameH5 -> IO r) -> IO r
withDataframeH5 file_id dfp = bracket (hkl_h5_open file_id dfp) hkl_h5_close

hkl_h5_open :: HId_t -> DataFrameH5Path -> IO DataFrameH5
hkl_h5_open file_id dp = DataFrameH5 file_id
                         <$> openH5Dataset' file_id (h5pImage dp)
                         <*> openH5Dataset' file_id (h5pMu dp)
                         <*> openH5Dataset' file_id (h5pOmega dp)
                         <*> openH5Dataset' file_id (h5pDelta dp)
                         <*> openH5Dataset' file_id (h5pGamma dp)
                         <*> openH5Dataset' file_id (h5pUB dp)
                         <*> openH5Dataset' file_id (h5pWavelength dp)
                         <*> openH5Dataset' file_id (h5pDiffractometerType dp)
  where
    openH5Dataset' hid (DataItem name _) = openH5Dataset hid name

hkl_h5_is_valid :: DataFrameH5 -> IO Bool
hkl_h5_is_valid df = do
  check_ndims' (h5mu df) 1
  check_ndims' (h5omega df) 1
  check_ndims' (h5delta df) 1
  check_ndims' (h5gamma df) 1
    where
      check_ndims' (Just dataset) target = check_ndims dataset target
      check_ndims' Nothing _ = return True

hkl_h5_close :: DataFrameH5 -> IO ()
hkl_h5_close d = do
  closeH5Dataset (h5image d)
  closeH5Dataset (h5mu d)
  closeH5Dataset (h5omega d)
  closeH5Dataset (h5delta d)
  closeH5Dataset (h5gamma d)
  closeH5Dataset (h5ub d)
  closeH5Dataset (h5wavelength d)
  closeH5Dataset (h5dtype d)

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
  mu <- get_position' (h5mu d) i
  omega <- get_position' (h5omega d) i
  delta <- get_position' (h5delta d) i
  gamma <- get_position' (h5gamma d) i
  wavelength <- get_position' (h5wavelength d) 0
  return DataFrame { df_n = i
                   , df_geometry = Geometry (Source (head wavelength *~ nano meter)) (mu ++ omega ++ delta ++ gamma)
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

  withH5File (root </> filename) $ \file_id ->
    withDataframeH5 file_id dataframe_h5p $ \dataframe_h5 -> do
      status <- hkl_h5_is_valid dataframe_h5
      runEffect $ getDataFrame dataframe_h5
        >->  Pipes.Prelude.print
