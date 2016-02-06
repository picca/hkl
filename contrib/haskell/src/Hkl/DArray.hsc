{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Hkl.DArray where

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
          return (Parameter name value (min, max))

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


geometryWavelengthGet :: Geometry -> IO Double
geometryWavelengthGet (Geometry g) =
  withForeignPtr g $ \gp -> do
    (CDouble d) <- c_hkl_geometry_wavelength_get gp unit
    return d

foreign import ccall unsafe "hkl.h hkl_geometry_wavelength_get"
  c_hkl_geometry_wavelength_get :: Ptr HklGeometry -- geometry
                                -> CInt -- unit
                                -> IO CDouble -- wavelength

geometryWavelengthSet :: Geometry -> Double -> IO ()
geometryWavelengthSet (Geometry g) w =
  withForeignPtr g $ \gp -> do
    let wavelength = CDouble w
    c_hkl_geometry_wavelength_set gp wavelength unit nullPtr

foreign import ccall unsafe "hkl.h hkl_geometry_wavelength_set"
  c_hkl_geometry_wavelength_set :: Ptr HklGeometry -- geometry
                                -> CDouble -- wavelength
                                -> CInt -- unit
                                -> Ptr () -- *gerror
                                -> IO () -- IO CInt but for now do not deal with the errors

geometryAxisNamesGet' :: Geometry -> IO [CString]
geometryAxisNamesGet' (Geometry g) =
  withForeignPtr g (c_hkl_geometry_axis_names_get >=> peekDArrayString)

foreign import ccall unsafe "hkl.h hkl_geometry_axis_names_get"
  c_hkl_geometry_axis_names_get :: Ptr HklGeometry -- goemetry
                                -> IO (Ptr ()) -- darray_string

geometryAxisGet :: Geometry -> CString -> IO Parameter
geometryAxisGet (Geometry g) n =
    withForeignPtr g $ \gp ->
        c_hkl_geometry_axis_get gp n nullPtr >>= peekParameter

foreign import ccall unsafe "hkl.h hkl_geometry_axis_get"
  c_hkl_geometry_axis_get :: Ptr HklGeometry -- geometry
                          -> CString -- axis name
                          -> Ptr () -- gerror
                          -> IO (Ptr HklParameter) -- parameter or nullPtr

geometryAxesGet :: Geometry -> IO [Parameter]
geometryAxesGet g = geometryAxisNamesGet' g >>= mapM (geometryAxisGet g)

geometryAxisValuesGet :: Geometry -> IO [Double]
geometryAxisValuesGet (Geometry g) =
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

geometryAxisValuesSet :: Geometry -> [Double] -> IO ()
geometryAxisValuesSet (Geometry g) v =
  withForeignPtr g $ \gp -> do
    darray <- c_hkl_geometry_axis_names_get gp
    n <- darrayStringLen darray
    withArray v $ \values -> do
      c_hkl_geometry_axis_values_set gp values n unit nullPtr

foreign import ccall unsafe "hkl.h hkl_geometry_axis_values_set"
  c_hkl_geometry_axis_values_set :: Ptr HklGeometry -- geometry
                                 -> Ptr Double -- axis values
                                 -> CSize -- size of axis values
                                 -> CInt -- unit
                                 -> Ptr () -- gerror
                                 -> IO () -- IO CInt but for now do not deal with the errors

-- engine

enginePseudoAxisNamesGet' :: HklEngine -> IO [CString]
enginePseudoAxisNamesGet' e = c_hkl_engine_pseudo_axis_names_get e >>= peekDArrayString

foreign import ccall unsafe "hkl.h hkl_engine_pseudo_axis_names_get"
  c_hkl_engine_pseudo_axis_names_get:: HklEngine -> IO (Ptr ()) -- darray_string

enginePseudoAxisNamesGet :: HklEngine -> IO [String]
enginePseudoAxisNamesGet e = enginePseudoAxisNamesGet' e >>= mapM peekCString

enginePseudoAxisGet :: HklEngine -> CString -> IO Parameter
enginePseudoAxisGet e n = c_hkl_engine_pseudo_axis_get e n nullPtr >>= peekParameter

foreign import ccall unsafe "hkl.h hkl_engine_pseudo_axis_get"
  c_hkl_engine_pseudo_axis_get:: HklEngine -> CString -> Ptr () -> IO (Ptr HklParameter)

foreign import ccall unsafe "hkl.h hkl_parameter_name_get"
  c_hkl_parameter_name_get:: Ptr HklParameter -> IO CString

foreign import ccall unsafe "hkl.h hkl_parameter_value_get"
  c_hkl_parameter_value_get:: Ptr HklParameter -> CInt -> IO Double

foreign import ccall unsafe "hkl.h hkl_parameter_min_max_get"
        c_hkl_parameter_min_max_get :: Ptr HklParameter -> Ptr Double -> Ptr Double -> CInt -> IO ()

enginePseudoAxesGet :: HklEngine -> IO [Parameter]
enginePseudoAxesGet e = enginePseudoAxisNamesGet' e >>= mapM (enginePseudoAxisGet e)

-- engineList

engineListEnginesGet :: EngineList -> IO [HklEngine]
engineListEnginesGet (EngineList e) = withForeignPtr e $ \ep -> do
  pdarray <- c_hkl_engine_list_engines_get ep
  n <- (#{peek darray_engine, size} pdarray) :: IO CSize
  engines <- #{peek darray_engine ,item} pdarray :: IO (Ptr HklEngine)
  peekArray (fromEnum n) engines

foreign import ccall unsafe "hkl.h hkl_engine_list_engines_get"
  c_hkl_engine_list_engines_get:: Ptr HklEngineList -> IO (Ptr ())

engineListPseudoAxesGet :: EngineList -> IO [[Parameter]]
engineListPseudoAxesGet l@(EngineList e) =
  withForeignPtr e $ \ep ->
      engineListEnginesGet l >>= mapM enginePseudoAxesGet
