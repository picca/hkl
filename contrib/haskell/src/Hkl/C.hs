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

factories :: IO (Map String HklFactory)
factories = do
  fs <- factoryGetAll
  ns <- mapM factoryNameGet fs
  return $ fromList $ zip ns fs

{-# NOINLINE factoryGetAll #-}
factoryGetAll :: IO [HklFactory]
factoryGetAll =  alloca $ \ptr -> do
                   fact <- c_hkl_factory_get_all ptr
                   n <- peek ptr
                   peekArray n fact

foreign import ccall unsafe "hkl.h hkl_factory_get_all"
  c_hkl_factory_get_all :: Ptr Int -> IO (Ptr HklFactory)

factoryNameGet :: HklFactory -> IO String
factoryNameGet factory = c_hkl_factory_name_get factory >>= peekCString

foreign import ccall unsafe "hkl.h hkl_factory_name_get"
  c_hkl_factory_name_get :: HklFactory -> IO CString


-- data Geometry

newGeometry :: HklFactory -> IO Geometry
newGeometry f =
  Geometry <$> (c_hkl_factory_create_new_geometry f >>= newForeignPtr c_hkl_geometry_free)

foreign import ccall unsafe "hkl.h hkl_factory_create_new_geometry"
  c_hkl_factory_create_new_geometry :: HklFactory -> IO (Ptr HklGeometry)

foreign import ccall unsafe "hkl.h &hkl_geometry_free"
  c_hkl_geometry_free :: FunPtr (Ptr HklGeometry -> IO ())

geometryName :: Geometry -> IO String
geometryName (Geometry g) = withForeignPtr g (c_hkl_geometry_name_get >=> peekCString)

foreign import ccall unsafe "hkl.h hkl_geometry_name_get"
  c_hkl_geometry_name_get :: Ptr HklGeometry -> IO CString

-- Engine

engineNameGet :: HklEngine -> IO String
engineNameGet engine = c_hkl_engine_name_get engine >>= peekCString

foreign import ccall unsafe "hkl.h hkl_engine_name_get"
  c_hkl_engine_name_get :: HklEngine -> IO CString


-- EngineList

newEngineList :: HklFactory -> IO EngineList
newEngineList f =
  EngineList <$> (c_hkl_factory_create_new_engine_list f >>= newForeignPtr c_hkl_engine_list_free)

foreign import ccall unsafe "hkl.h hkl_factory_create_new_engine_list"
  c_hkl_factory_create_new_engine_list:: HklFactory -> IO (Ptr HklEngineList)

foreign import ccall unsafe "hkl.h &hkl_engine_list_free"
  c_hkl_engine_list_free :: FunPtr (Ptr HklEngineList -> IO ())

engineListGet :: EngineList -> IO ()
engineListGet (EngineList e) = withForeignPtr e c_hkl_engine_list_get

foreign import ccall unsafe "hkl.h hkl_engine_list_get"
  c_hkl_engine_list_get:: Ptr HklEngineList -> IO ()

engineListInit :: EngineList -> Geometry -> Detector -> Sample -> IO ()
engineListInit (EngineList e) (Geometry g) (Detector d) (Sample s) =
  withForeignPtr s $ \sp ->
      withForeignPtr d $ \dp ->
          withForeignPtr g $ \gp ->
              withForeignPtr e $ \ep ->
                  c_hkl_engine_list_init ep gp dp sp


foreign import ccall unsafe "hkl.h hkl_engine_list_init"
  c_hkl_engine_list_init:: Ptr HklEngineList -> Ptr HklGeometry -> Ptr HklDetector -> Ptr HklSample -> IO ()

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
