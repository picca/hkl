{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hkl.H5
    ( Dataset
    , File
    , check_ndims
    , closeDataset
    , get_position
    , get_position'
    , lenH5Dataspace
    , openDataset
    , pack
    , withH5File
    )
    where


import Bindings.HDF5.File (File, AccFlags(ReadOnly), openFile, closeFile)
import Bindings.HDF5.Dataset (Dataset, openDataset, closeDataset)
import Bindings.HDF5.Core (hid)
import Bindings.HDF5.Raw
-- import Control.Applicative
import Control.Exception (bracket)
import Control.Monad (void)
import Data.ByteString.Char8 (pack)
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

check_ndims :: Dataset -> Int -> IO Bool
check_ndims d expected = do
  space_id <- h5d_get_space (hid d)
  (CInt ndims) <- h5s_get_simple_extent_ndims space_id
  return $ expected == fromEnum ndims

-- DataType

newtype H5DataType = H5DataType HId_t
                   deriving (Show, HId)

withH5DataType :: Dataset -> (Maybe H5DataType -> IO r) -> IO r
withH5DataType d = bracket acquire release
  where
    acquire = do
      type_id <- h5d_get_type (hid d)
      return $ if isError type_id then Nothing else Just (fromHId type_id)
    release = maybe (return $ HErr_t (-1)) (h5t_close . toHId)

get_position :: Dataset -> Int -> IO ([Double], HErr_t)
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
                h5d_read (hid d) (toHId mem_type_id) (toHId mem_space_id) (toHId space_id) h5p_DEFAULT rdata

get_position' :: Dataset -> Int -> IO [Double]
get_position' d idx = do
  (positions, HErr_t status) <- get_position d idx
  if status < 0 then do
                  (failovers, HErr_t status') <- get_position d 0
                  return $ if status' < 0 then [0.0] else failovers
  else return $ if status < 0 then [0.0] else positions

-- | File

withH5File :: FilePath -> (File -> IO r) -> IO r
withH5File fp f = do
  h5file <- openFile (pack fp) [ReadOnly] Nothing
  bracket (return h5file) closeFile f

-- | Dataspace

newtype H5Dataspace = H5Dataspace HId_t
                      deriving (Show, HId)

-- check how to merge both methods

withDataspace' :: (Maybe H5Dataspace -> IO r) -> IO r
withDataspace' = bracket acquire release
  where
    acquire = do
      _hid <-
        withInList [HSize_t 1] $ \current_dims ->
        withInList [HSize_t 1] $ \maximum_dims ->
        h5s_create_simple 1 current_dims maximum_dims
      return $ if isError _hid then Nothing else Just (fromHId _hid)
    release = maybe  (return $ HErr_t (-1)) (h5s_close . toHId)

withDataspace :: Dataset -> (Maybe H5Dataspace -> IO r) -> IO r
withDataspace d = bracket acquire release
  where
    acquire = do
      _hid <- h5d_get_space (hid d)
      return  $ if isError _hid then Nothing else Just (fromHId _hid)
    release = maybe (return $ HErr_t (-1)) (h5s_close . toHId)

lenH5Dataspace :: Dataset -> IO (Maybe Int)
lenH5Dataspace = withDataspace'' (maybe (return Nothing) len)
  where
    withDataspace'' f d = withDataspace d f
    len space_id = do
      (HSSize_t n) <- h5s_get_simple_extent_npoints (toHId space_id)
      return $ if n < 0 then Nothing else Just (fromIntegral n)
