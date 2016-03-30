module Hkl.H5
    ( check_ndims
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

check_ndims :: HId_t -> Int -> IO Bool
check_ndims hid expected = do
  space_id <- h5d_get_space hid
  (CInt ndims) <- h5s_get_simple_extent_ndims space_id
  return $ expected == fromEnum ndims

-- DataType

withH5DataType :: HId_t -> (Maybe HId_t -> IO r) -> IO r
withH5DataType hid = bracket acquire release
  where
    acquire = do
      type_id@(HId_t status) <- h5d_get_type hid
      return  $ if status < 0 then Nothing else (Just type_id)
    release (Just thid) = h5t_close thid
    release Nothing = return (HErr_t (-1))

get_position :: HId_t -> Int -> IO ([Double], HErr_t)
get_position hid n =
  withH5DataType hid $ \mmem_type_id -> case mmem_type_id of
    (Just mem_type_id) -> withDataspace hid $ \mspace_id -> case mspace_id of
        (Just space_id) -> do
          void $ withInList [HSize_t (fromIntegral n)] $ \start ->
            withInList [HSize_t 1] $ \stride ->
            withInList [HSize_t 1] $ \count ->
            withInList [HSize_t 1] $ \block ->
            h5s_select_hyperslab space_id h5s_SELECT_SET start stride count block
          withDataspace' $ \mmem_space_id -> case mmem_space_id of
            (Just mem_space_id) -> withOutList 1 $ \rdata ->
              h5d_read hid mem_type_id mem_space_id space_id h5p_DEFAULT rdata
            Nothing -> return failed
        Nothing -> return failed
    Nothing -> return failed
    where
      failed = ([], HErr_t (-1))

get_position' :: Maybe HId_t -> Int -> IO [Double]
get_position' dataset idx = case dataset of
        (Just dataset') -> do
          (positions, HErr_t status) <- get_position dataset' idx
          if status < 0 then do
             (failovers, HErr_t status') <- get_position dataset' 0
             return $ if status' < 0 then [0.0] else failovers
          else return $ if status < 0 then [0.0] else positions
        Nothing -> return [0.0]

-- | File

withH5File :: FilePath -> (HId_t -> IO r) -> IO r
withH5File fp f = do
  mhid <- openH5File fp
  case mhid of
    Just hid -> bracket (return $ id hid) h5f_close f
    Nothing -> error $ "Can not read the h5 file: " ++ fp

openH5File :: FilePath -> IO (Maybe HId_t)
openH5File fp = do
  hid@(HId_t status) <- withCString fp (\file -> h5f_open file h5f_ACC_RDONLY h5p_DEFAULT)
  return $ if status < 0 then Nothing else Just hid

-- | Dataset

type DatasetPath = String

openH5Dataset :: HId_t -> DatasetPath -> IO (Maybe HId_t)
openH5Dataset h dp = do
  hid@(HId_t status) <- withCString dp (\dataset -> h5d_open2 h dataset h5p_DEFAULT)
  return $ if status < 0 then Nothing else Just hid

closeH5Dataset :: Maybe HId_t -> IO ()
closeH5Dataset mhid = case mhid of
  (Just hid) -> void $ h5d_close hid
  Nothing -> return ()

-- | Dataspace
-- check how to merge both methods

withDataspace' :: (Maybe HId_t -> IO r) -> IO r
withDataspace' f = bracket acquire release f
  where
    acquire = do
      space_id@(HId_t status) <- withInList [HSize_t 1] $ \current_dims ->
        withInList [HSize_t 1] $ \maximum_dims ->
        h5s_create_simple 1 current_dims maximum_dims
      return $ if status < 0 then Nothing else (Just space_id)
    release (Just shid) = h5s_close shid
    release Nothing = return (HErr_t (-1))

withDataspace :: HId_t -> (Maybe HId_t -> IO r) -> IO r
withDataspace hid f = bracket acquire release f
  where
    acquire = do
      space_id@(HId_t status) <- h5d_get_space hid
      return  $ if status < 0 then Nothing else (Just space_id)
    release (Just shid) = h5s_close shid
    release Nothing = return (HErr_t (-1))

lenH5Dataspace :: Maybe HId_t -> IO (Maybe Int)
lenH5Dataspace mhid =  case mhid of
  (Just hid) -> withDataspace hid len
  Nothing -> return Nothing
  where
    len (Just space_id) = do
      (HSSize_t n) <- h5s_get_simple_extent_npoints space_id
      return $ if n < 0 then Nothing else (Just (fromIntegral n))
    len Nothing = return Nothing
