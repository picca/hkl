{-# LANGUAGE ForeignFunctionInterface #-}

module Hkl.C where

import Control.Applicative ((<$>))
import Control.Monad ((>=>))
import Data.Map.Strict as Map
import Foreign
import Foreign.C
import Foreign.C.Types

import Hkl.Types

-- data Factory

factories :: IO (Map String Factory)
factories = do
  fs <- factoryGetAll
  ns <- mapM factoryNameGet fs
  return $ fromList $ zip ns fs

{-# NOINLINE factoryGetAll #-}
factoryGetAll :: IO [Factory]
factoryGetAll =  alloca $ \ptr -> do
                   fact <- c_hkl_factory_get_all ptr
                   n <- peek ptr
                   hklfactories <- peekArray n fact
                   return $ fmap Factory hklfactories

foreign import ccall unsafe "hkl.h hkl_factory_get_all"
  c_hkl_factory_get_all :: Ptr Int -> IO (Ptr (Ptr HklFactory))

factoryNameGet :: Factory -> IO String
factoryNameGet (Factory factory) = c_hkl_factory_name_get factory >>= peekCString

foreign import ccall unsafe "hkl.h hkl_factory_name_get"
  c_hkl_factory_name_get :: Ptr HklFactory -> IO CString


-- Sample

newSample :: String -> IO (Maybe Sample)
newSample name = withCString name $ \cname -> do
                   ptr <- c_hkl_sample_new cname
                   if ptr /= nullPtr
                   then do
                     foreignPtr <- newForeignPtr c_hkl_sample_free ptr
                     let sample = Sample foreignPtr
                     return $ Just sample
                   else
                     return Nothing

foreign import ccall unsafe "hkl.h hkl_sample_new"
  c_hkl_sample_new:: CString -> IO (Ptr HklSample)

foreign import ccall unsafe "hkl.h &hkl_sample_free"
  c_hkl_sample_free :: FunPtr (Ptr HklSample -> IO ())
