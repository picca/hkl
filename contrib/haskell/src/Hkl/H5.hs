{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hkl.H5
    ( H5Dataset
    , H5File
    , check_ndims
    , closeH5Dataset
    , get_position
    , get_position'
    , lenH5Dataspace
    , openH5Dataset
    , withH5File
    )
    where

import Bindings.HDF5.Raw
-- import Control.Applicative
import Control.Exception (bracket)
import Control.Monad (void)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt(..))
-- import Foreign.Ptr
import Foreign.Ptr.Conventions (withInList, withOutList)

{-# ANN module "HLint: ignore Use camelCase" #-}


class HId t where
  toHId :: t -> HId_t
  fromHId :: HId_t -> t
  isError :: t -> Bool

instance HId HId_t where
  toHId = id
  fromHId = id
  isError = (< HId_t 0)


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

check_ndims :: H5Dataset -> Int -> IO Bool
check_ndims d expected = do
  space_id <- h5d_get_space (toHId d)
  (CInt ndims) <- h5s_get_simple_extent_ndims space_id
  return $ expected == fromEnum ndims

-- DataType

newtype H5DataType = H5DataType HId_t
                   deriving (Show, HId)

withH5DataType :: H5Dataset -> (Maybe H5DataType -> IO r) -> IO r
withH5DataType d = bracket acquire release
  where
    acquire = do
      type_id <- h5d_get_type (toHId d)
      return $ if (isError type_id) then Nothing else Just (fromHId type_id)
    release = maybe (return $ HErr_t (-1)) (h5t_close . toHId)

get_position :: H5Dataset -> Int -> IO ([Double], HErr_t)
get_position d n = withH5DataType d (maybe default_ read''')
  where
    default_ = return ([], HErr_t (-1))
    read''' mem_type_id = withDataspace d (maybe default_ read'')
      where
        read'' space_id = do
          void $
            withInList [HSize_t (fromIntegral n)] $ \start ->
            withInList [HSize_t 1] $ \stride ->
            withInList [HSize_t 1] $ \count ->
            withInList [HSize_t 1] $ \block ->
            h5s_select_hyperslab (toHId space_id) h5s_SELECT_SET start stride count block
          withDataspace' (maybe default_ read')
            where
              read' mem_space_id = withOutList 1 $ \rdata ->
                h5d_read (toHId d) (toHId mem_type_id) (toHId mem_space_id) (toHId space_id) h5p_DEFAULT rdata

get_position' :: Maybe H5Dataset -> Int -> IO [Double]
get_position' md idx = maybe default_ get_positions'' md
  where
    default_ = return [0.0]
    get_positions'' d = do
      (positions, HErr_t status) <- get_position d idx
      if status < 0 then do
        (failovers, HErr_t status') <- get_position d 0
        return $ if status' < 0 then [0.0] else failovers
      else return $ if status < 0 then [0.0] else positions

-- | File

newtype H5File = H5File HId_t
               deriving (Show, HId)

withH5File :: FilePath -> (H5File -> IO r) -> IO r
withH5File fp f = do
  mh5file <- openH5File fp
  maybe (return $ error $ "Can not read the h5 file: " ++ fp) go mh5file
    where
      go h5file = bracket (return h5file) (h5f_close . toHId) f

openH5File :: FilePath -> IO (Maybe H5File)
openH5File fp = do
  hid <- withCString fp (\file -> h5f_open file h5f_ACC_RDONLY h5p_DEFAULT)
  return $ if (isError hid) then Nothing else Just (fromHId hid)

-- | Dataset

newtype H5Dataset = H5Dataset HId_t
                    deriving (Show, HId)

type DatasetPath = String

openH5Dataset :: H5File -> DatasetPath -> IO (Maybe H5Dataset)
openH5Dataset h5file dp = do
  hid <- withCString dp (\dataset -> h5d_open2 (toHId h5file) dataset h5p_DEFAULT)
  return $ if (isError hid) then Nothing else Just (fromHId hid)

closeH5Dataset :: Maybe H5Dataset -> IO ()
closeH5Dataset = maybe (return ()) (void . h5d_close . toHId)

-- | Dataspace

newtype H5Dataspace = H5Dataspace HId_t
                      deriving (Show, HId)

-- check how to merge both methods

withDataspace' :: (Maybe H5Dataspace -> IO r) -> IO r
withDataspace' = bracket acquire release
  where
    acquire = do
      hid <-
        withInList [HSize_t 1] $ \current_dims ->
        withInList [HSize_t 1] $ \maximum_dims ->
        h5s_create_simple 1 current_dims maximum_dims
      return $ if (isError hid) then Nothing else Just (fromHId hid)
    release = maybe  (return $ HErr_t (-1)) (h5s_close . toHId)

withDataspace :: H5Dataset -> (Maybe H5Dataspace -> IO r) -> IO r
withDataspace d = bracket acquire release
  where
    acquire = do
      hid <- h5d_get_space (toHId d)
      return  $ if (isError hid) then Nothing else Just (fromHId hid)
    release = maybe (return $ HErr_t (-1)) (h5s_close . toHId)

lenH5Dataspace :: Maybe H5Dataset -> IO (Maybe Int)
lenH5Dataspace = maybe (return Nothing) (withDataspace'' (maybe (return Nothing) len))
  where
    withDataspace'' f d = withDataspace d f
    len space_id = do
      (HSSize_t n) <- h5s_get_simple_extent_npoints (toHId space_id)
      return $ if n < 0 then Nothing else Just (fromIntegral n)
