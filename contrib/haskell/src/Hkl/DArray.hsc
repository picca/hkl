{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Hkl.DArray where

import Control.Monad
import Foreign
import Foreign.C

import Hkl.Types

#include "hkl.h"

engineListEnginesGet :: EngineList -> IO [Engine]
engineListEnginesGet (EngineList e) = withForeignPtr e $ \ep -> do
  pdarray <- c_hkl_engine_list_engines_get ep
  n <- (#{peek darray_engine, size} pdarray) :: IO CSize
  engines <- #{peek darray_engine ,item} pdarray :: IO (Ptr Engine)
  peekArray (fromEnum n) engines

foreign import ccall unsafe "hkl.h hkl_engine_list_engines_get"
  c_hkl_engine_list_engines_get:: Ptr HklEngineList -> IO (Ptr ())
