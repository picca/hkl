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


-- Engine

engineNameGet :: Engine -> IO String
engineNameGet (Engine engine) = c_hkl_engine_name_get engine >>= peekCString

foreign import ccall unsafe "hkl.h hkl_engine_name_get"
  c_hkl_engine_name_get :: Ptr HklEngine -> IO CString

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

-- Detector

newDetector :: DetectorType -> IO Detector
newDetector t =
  Detector <$> (c_hkl_detector_new it >>= newForeignPtr c_hkl_detector_free)
      where
        it = case t of
             DetectorType0D -> 0

foreign import ccall unsafe "hkl.h hkl_detector_new"
  c_hkl_detector_new:: CInt -> IO (Ptr HklDetector)

foreign import ccall unsafe "hkl.h &hkl_detector_free"
  c_hkl_detector_free :: FunPtr (Ptr HklDetector -> IO ())
