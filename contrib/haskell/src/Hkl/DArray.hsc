{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Hkl.DArray
    ( engineListPseudoAxesGet
    ) where

import Control.Monad
import Foreign
import Foreign.C

import Hkl.Types

#include "hkl.h"

engineListEnginesGet :: EngineList -> IO [HklEngine]
engineListEnginesGet (EngineList e) = withForeignPtr e $ \ep -> do
  pdarray <- c_hkl_engine_list_engines_get ep
  n <- (#{peek darray_engine, size} pdarray) :: IO CSize
  engines <- #{peek darray_engine ,item} pdarray :: IO (Ptr HklEngine)
  peekArray (fromEnum n) engines

foreign import ccall unsafe "hkl.h hkl_engine_list_engines_get"
  c_hkl_engine_list_engines_get:: Ptr HklEngineList -> IO (Ptr ())

enginePseudoAxisNamesGet' :: HklEngine -> IO [CString]
enginePseudoAxisNamesGet' e = do
  pdarray <- c_hkl_engine_pseudo_axis_names_get e
  n <- (#{peek darray_string, size} pdarray) :: IO CSize
  items <- #{peek darray_string ,item} pdarray :: IO (Ptr CString)
  peekArray (fromEnum n) items

enginePseudoAxisNamesGet :: HklEngine -> IO [String]
enginePseudoAxisNamesGet e = do
  cnames <- enginePseudoAxisNamesGet' e
  mapM peekCString cnames

enginePseudoAxisGet :: HklEngine -> CString -> IO Parameter
enginePseudoAxisGet e n = do
  pseudoAxis <- c_hkl_engine_pseudo_axis_get e n nullPtr
  cname <- c_hkl_parameter_name_get pseudoAxis
  name <- peekCString cname
  return (Parameter name)

enginePseudoAxesGet :: HklEngine -> IO [Parameter]
enginePseudoAxesGet e = do
  cnames <- enginePseudoAxisNamesGet' e
  mapM (enginePseudoAxisGet e) cnames

foreign import ccall unsafe "hkl.h hkl_engine_pseudo_axis_names_get"
  c_hkl_engine_pseudo_axis_names_get:: HklEngine -> IO (Ptr ()) -- darray_string

foreign import ccall unsafe "hkl.h hkl_engine_pseudo_axis_get"
  c_hkl_engine_pseudo_axis_get:: HklEngine -> CString -> Ptr () -> IO (Ptr HklParameter)

foreign import ccall unsafe "hkl.h hkl_parameter_name_get"
  c_hkl_parameter_name_get:: Ptr HklParameter -> IO CString

engineListPseudoAxesGet :: EngineList -> IO [[Parameter]]
engineListPseudoAxesGet e = do
      engines <- engineListEnginesGet e
      mapM enginePseudoAxesGet engines
