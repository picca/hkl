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


import Bindings.HDF5.Core ( hid
                          , HSize(..)
                          )
import Bindings.HDF5.File ( File
                          , AccFlags(ReadOnly)
                          , openFile
                          , closeFile
                          )
import Bindings.HDF5.Dataset ( Dataset
                             , openDataset
                             , closeDataset
                             , getDatasetSpace
                             , getDatasetType
                             )
import Bindings.HDF5.Dataspace ( Dataspace
                               , SelectionOperator(Set)
                               , closeDataspace
                               , createSimpleDataspace
                               , getSimpleDataspaceExtentNDims
                               , getSimpleDataspaceExtentNPoints
                               , selectHyperslab
                               )
import Bindings.HDF5.Datatype ( Datatype
                              , closeTypeID
                              )
import Bindings.HDF5.Raw
-- import Control.Applicative
import Control.Exception (bracket)
import Data.ByteString.Char8 (pack)
import Foreign.C.Types (CInt(..))
-- import Foreign.Ptr
import Foreign.Ptr.Conventions (withOutList)

{-# ANN module "HLint: ignore Use camelCase" #-}

check_ndims :: Dataset -> Int -> IO Bool
check_ndims d expected = do
  space_id <- getDatasetSpace d
  (CInt ndims) <- getSimpleDataspaceExtentNDims space_id
  return $ expected == fromEnum ndims

-- DataType

withH5DataType :: Dataset -> (Datatype -> IO r) -> IO r
withH5DataType d = bracket acquire release
  where
    acquire = getDatasetType d
    release = closeTypeID

get_position :: Dataset -> Int -> IO ([Double], HErr_t)
get_position d n = withH5DataType d read'''
  where
    read''' mem_type_id = withDataspace d read''
      where
        read'' space_id = do
          let start = HSize (fromIntegral n)
          let stride = Just (HSize 1)
          let count = HSize 1
          let block = Just (HSize 1)
          selectHyperslab space_id Set [(start, stride, count, block)]
          withDataspace' read'
            where
              read' mem_space_id = withOutList 1 $ \rdata ->
                h5d_read (hid d) (hid mem_type_id) (hid mem_space_id) (hid space_id) h5p_DEFAULT rdata

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

-- check how to merge both methods

withDataspace' :: (Dataspace -> IO r) -> IO r
withDataspace' = bracket acquire release
  where
    acquire = createSimpleDataspace [HSize 1]
    release = closeDataspace

withDataspace :: Dataset -> (Dataspace -> IO r) -> IO r
withDataspace d = bracket acquire release
  where
    acquire = getDatasetSpace d
    release = closeDataspace

lenH5Dataspace :: Dataset -> IO (Maybe Int)
lenH5Dataspace = withDataspace'' len
  where
    withDataspace'' f d = withDataspace d f
    len space_id = do
      (HSize n) <- getSimpleDataspaceExtentNPoints space_id
      return $ if n < 0 then Nothing else Just (fromIntegral n)
