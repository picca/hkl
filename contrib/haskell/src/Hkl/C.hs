{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hkl.C
       ( Factory
       , DetectorType (..)
       , engineListInit
       , factories
       , geometryName
       , newDetector
       , newEngineList
       , newGeometry
       , newSample
       ) where

import Control.Monad ((>=>))
import Data.Map.Strict as Map
import Foreign (Ptr, ForeignPtr, FunPtr, peek, newForeignPtr, withForeignPtr)
import Foreign.C.Types
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (Storable)


-- data Factory
newtype Factory = Factory (Ptr Factory) deriving (Show, Storable)

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
                   peekArray n fact

foreign import ccall unsafe "hkl.h hkl_factory_get_all"
  c_hkl_factory_get_all :: Ptr Int -> IO (Ptr Factory)

factoryNameGet :: Factory -> IO String
factoryNameGet factory = c_hkl_factory_name_get factory >>= peekCString

foreign import ccall unsafe "hkl.h hkl_factory_name_get"
  c_hkl_factory_name_get :: Factory -> IO CString


-- data Geometry
data HklGeometry
newtype Geometry = Geometry (ForeignPtr HklGeometry) deriving (Show)

newGeometry :: Factory -> IO Geometry
newGeometry f =
  Geometry <$> (c_hkl_factory_create_new_geometry f >>= newForeignPtr c_hkl_geometry_free)

foreign import ccall unsafe "hkl.h hkl_factory_create_new_geometry"
  c_hkl_factory_create_new_geometry :: Factory -> IO (Ptr HklGeometry)

foreign import ccall unsafe "hkl.h &hkl_geometry_free"
  c_hkl_geometry_free :: FunPtr (Ptr HklGeometry -> IO ())

geometryName :: Geometry -> IO String
geometryName (Geometry g) = withForeignPtr g (c_hkl_geometry_name_get >=> peekCString)

foreign import ccall unsafe "hkl.h hkl_geometry_name_get"
  c_hkl_geometry_name_get :: Ptr HklGeometry -> IO CString


-- EngineList
data HklEngineList
newtype EngineList = EngineList (ForeignPtr HklEngineList) deriving (Show)

newEngineList :: Factory -> IO EngineList
newEngineList f =
  EngineList <$> (c_hkl_factory_create_new_engine_list f >>= newForeignPtr c_hkl_engine_list_free)

foreign import ccall unsafe "hkl.h hkl_factory_create_new_engine_list"
  c_hkl_factory_create_new_engine_list:: Factory -> IO (Ptr HklEngineList)

foreign import ccall unsafe "hkl.h &hkl_engine_list_free"
  c_hkl_engine_list_free :: FunPtr (Ptr HklEngineList -> IO ())

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
data HklSample
newtype Sample = Sample (ForeignPtr HklSample) deriving (Show)

newSample :: String -> IO Sample
newSample name =
  Sample <$> withCString name (c_hkl_sample_new >=> newForeignPtr c_hkl_sample_free)

foreign import ccall unsafe "hkl.h hkl_sample_new"
  c_hkl_sample_new:: CString -> IO (Ptr HklSample)

foreign import ccall unsafe "hkl.h &hkl_sample_free"
  c_hkl_sample_free :: FunPtr (Ptr HklSample -> IO ())


-- Detector
data DetectorType = DetectorType0D

data HklDetector
newtype Detector = Detector (ForeignPtr HklDetector) deriving (Show)

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
