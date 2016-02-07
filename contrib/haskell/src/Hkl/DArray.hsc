{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Hkl.DArray
       ( compute ) where

import Control.Monad
import Foreign
import Foreign.C

import Hkl.Types

#include "hkl.h"

-- helpers

peekParameter :: Ptr HklParameter -> IO Parameter
peekParameter p =
    alloca $ \pmin ->
        alloca $ \pmax -> do
          cname <- c_hkl_parameter_name_get p
          name <- peekCString cname
          value <- c_hkl_parameter_value_get p unit
          c_hkl_parameter_min_max_get p pmin pmax unit
          min <- peek pmin
          max <- peek pmax
          return (Parameter name value (Range min max))

peekDArrayString :: Ptr () -> IO [CString]
peekDArrayString p = do
  n <- (#{peek darray_string, size} p) :: IO CSize
  items <- #{peek darray_string ,item} p :: IO (Ptr CString)
  peekArray (fromEnum n) items

darrayStringLen :: Ptr () -> IO (CSize)
darrayStringLen p = do
    n <- (#{peek darray_string, size} p) :: IO CSize
    return n

-- geometry

newGeometry :: Factory -> Geometry -> IO (ForeignPtr HklGeometry)
newGeometry (Factory f) (Geometry (Source w) vs) = do
  let wavelength = CDouble w
  geometry <- c_hkl_factory_create_new_geometry f
  c_hkl_geometry_wavelength_set geometry wavelength unit nullPtr
  darray <- c_hkl_geometry_axis_names_get geometry
  n <- darrayStringLen darray
  withArray vs $ \values -> do
      c_hkl_geometry_axis_values_set geometry values n unit nullPtr

  fptr <- newForeignPtr c_hkl_geometry_free geometry
  return fptr

foreign import ccall unsafe "hkl.h hkl_factory_create_new_geometry"
  c_hkl_factory_create_new_geometry :: Ptr HklFactory -> IO (Ptr HklGeometry)

foreign import ccall unsafe "hkl.h &hkl_geometry_free"
  c_hkl_geometry_free :: FunPtr (Ptr HklGeometry -> IO ())

foreign import ccall unsafe "hkl.h hkl_geometry_wavelength_set"
  c_hkl_geometry_wavelength_set :: Ptr HklGeometry -- geometry
                                -> CDouble -- wavelength
                                -> CInt -- unit
                                -> Ptr () -- *gerror
                                -> IO () -- IO CInt but for now do not deal with the errors

foreign import ccall unsafe "hkl.h hkl_geometry_axis_values_set"
  c_hkl_geometry_axis_values_set :: Ptr HklGeometry -- geometry
                                 -> Ptr Double -- axis values
                                 -> CSize -- size of axis values
                                 -> CInt -- unit
                                 -> Ptr () -- gerror
                                 -> IO () -- IO CInt but for now do not deal with the errors

geometryName :: ForeignPtr HklGeometry -> IO String
geometryName g = withForeignPtr g (c_hkl_geometry_name_get >=> peekCString)

foreign import ccall unsafe "hkl.h hkl_geometry_name_get"
  c_hkl_geometry_name_get :: Ptr HklGeometry -> IO CString


geometryWavelengthGet :: ForeignPtr HklGeometry -> IO Double
geometryWavelengthGet g =
  withForeignPtr g $ \gp -> do
    (CDouble d) <- c_hkl_geometry_wavelength_get gp unit
    return d

foreign import ccall unsafe "hkl.h hkl_geometry_wavelength_get"
  c_hkl_geometry_wavelength_get :: Ptr HklGeometry -- geometry
                                -> CInt -- unit
                                -> IO CDouble -- wavelength

geometryAxisNamesGet' :: ForeignPtr HklGeometry -> IO [CString]
geometryAxisNamesGet' g =
  withForeignPtr g (c_hkl_geometry_axis_names_get >=> peekDArrayString)

foreign import ccall unsafe "hkl.h hkl_geometry_axis_names_get"
  c_hkl_geometry_axis_names_get :: Ptr HklGeometry -- goemetry
                                -> IO (Ptr ()) -- darray_string

geometryAxisGet :: ForeignPtr HklGeometry -> CString -> IO Parameter
geometryAxisGet g n =
    withForeignPtr g $ \gp ->
        c_hkl_geometry_axis_get gp n nullPtr >>= peekParameter

foreign import ccall unsafe "hkl.h hkl_geometry_axis_get"
  c_hkl_geometry_axis_get :: Ptr HklGeometry -- geometry
                          -> CString -- axis name
                          -> Ptr () -- gerror
                          -> IO (Ptr HklParameter) -- parameter or nullPtr

geometryAxesGet :: ForeignPtr HklGeometry -> IO [Parameter]
geometryAxesGet g = geometryAxisNamesGet' g >>= mapM (geometryAxisGet g)

geometryAxisValuesGet :: ForeignPtr HklGeometry -> IO [Double]
geometryAxisValuesGet g =
  withForeignPtr g $ \gp -> do
    darray <- c_hkl_geometry_axis_names_get gp
    n <- darrayStringLen darray
    let nn = fromEnum n
    allocaArray nn $ \values -> do
      c_hkl_geometry_axis_values_get gp values n unit
      peekArray nn values

foreign import ccall unsafe "hkl.h hkl_geometry_axis_values_get"
  c_hkl_geometry_axis_values_get :: Ptr HklGeometry -- geometry
                                 -> Ptr Double -- axis values
                                 -> CSize -- size of axis values
                                 -> CInt -- unit
                                 -> IO () -- IO CInt but for now do not deal with the errors

-- Detector

newDetector :: Detector -> IO (ForeignPtr HklDetector)
newDetector (Detector t) =
  (c_hkl_detector_new it >>= newForeignPtr c_hkl_detector_free)
      where
        it = case t of
             DetectorType0D -> 0

foreign import ccall unsafe "hkl.h hkl_detector_new"
  c_hkl_detector_new:: CInt -> IO (Ptr HklDetector)

foreign import ccall unsafe "hkl.h &hkl_detector_free"
  c_hkl_detector_free :: FunPtr (Ptr HklDetector -> IO ())

-- engine

enginePseudoAxisNamesGet' :: Engine -> IO [CString]
enginePseudoAxisNamesGet' (Engine e) = c_hkl_engine_pseudo_axis_names_get e >>= peekDArrayString

foreign import ccall unsafe "hkl.h hkl_engine_pseudo_axis_names_get"
  c_hkl_engine_pseudo_axis_names_get:: Ptr HklEngine -> IO (Ptr ()) -- darray_string

enginePseudoAxisNamesGet :: Engine -> IO [String]
enginePseudoAxisNamesGet e = enginePseudoAxisNamesGet' e >>= mapM peekCString

enginePseudoAxisGet :: Engine -> CString -> IO Parameter
enginePseudoAxisGet (Engine e) n = c_hkl_engine_pseudo_axis_get e n nullPtr >>= peekParameter

foreign import ccall unsafe "hkl.h hkl_engine_pseudo_axis_get"
  c_hkl_engine_pseudo_axis_get:: Ptr HklEngine -> CString -> Ptr () -> IO (Ptr HklParameter)

foreign import ccall unsafe "hkl.h hkl_parameter_name_get"
  c_hkl_parameter_name_get:: Ptr HklParameter -> IO CString

foreign import ccall unsafe "hkl.h hkl_parameter_value_get"
  c_hkl_parameter_value_get:: Ptr HklParameter -> CInt -> IO Double

foreign import ccall unsafe "hkl.h hkl_parameter_min_max_get"
  c_hkl_parameter_min_max_get :: Ptr HklParameter -> Ptr Double -> Ptr Double -> CInt -> IO ()

enginePseudoAxesGet :: Engine -> IO [Parameter]
enginePseudoAxesGet e = enginePseudoAxisNamesGet' e >>= mapM (enginePseudoAxisGet e)

-- EngineList

newEngineList :: Factory -> IO EngineList
newEngineList (Factory f) =
  EngineList <$> (c_hkl_factory_create_new_engine_list f >>= newForeignPtr c_hkl_engine_list_free)

foreign import ccall unsafe "hkl.h hkl_factory_create_new_engine_list"
  c_hkl_factory_create_new_engine_list:: Ptr HklFactory -> IO (Ptr HklEngineList)

foreign import ccall unsafe "hkl.h &hkl_engine_list_free"
  c_hkl_engine_list_free :: FunPtr (Ptr HklEngineList -> IO ())

engineListGet :: EngineList -> IO ()
engineListGet (EngineList e) = withForeignPtr e c_hkl_engine_list_get

foreign import ccall unsafe "hkl.h hkl_engine_list_get"
  c_hkl_engine_list_get:: Ptr HklEngineList -> IO ()

engineListInit :: EngineList
               -> ForeignPtr HklGeometry
               -> ForeignPtr HklDetector
               -> Sample
               -> IO ()
engineListInit (EngineList e) g d (Sample s) =
  withForeignPtr s $ \sp ->
      withForeignPtr d $ \dp ->
          withForeignPtr g $ \gp ->
              withForeignPtr e $ \ep ->
                  c_hkl_engine_list_init ep gp dp sp


foreign import ccall unsafe "hkl.h hkl_engine_list_init"
  c_hkl_engine_list_init:: Ptr HklEngineList -> Ptr HklGeometry -> Ptr HklDetector -> Ptr HklSample -> IO ()

engineListEnginesGet :: EngineList -> IO [Engine]
engineListEnginesGet (EngineList e) = withForeignPtr e $ \ep -> do
  pdarray <- c_hkl_engine_list_engines_get ep
  n <- (#{peek darray_engine, size} pdarray) :: IO CSize
  engines <- #{peek darray_engine ,item} pdarray :: IO (Ptr (Ptr HklEngine))
  es <- peekArray (fromEnum n) engines
  return $ fmap Engine es

foreign import ccall unsafe "hkl.h hkl_engine_list_engines_get"
  c_hkl_engine_list_engines_get:: Ptr HklEngineList -> IO (Ptr ())

engineListPseudoAxesGet :: EngineList -> IO [[Parameter]]
engineListPseudoAxesGet l@(EngineList e) =
  withForeignPtr e $ \ep ->
      engineListEnginesGet l >>= mapM enginePseudoAxesGet

compute :: Factory -> Geometry -> Detector -> Sample -> IO [[Parameter]]
compute factory g d sample = do
  engines <- newEngineList factory
  geometry <- newGeometry factory g
  detector <- newDetector d
  engineListInit engines geometry detector sample
  engineListGet engines
  pss <- engineListPseudoAxesGet engines
  return pss
