{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Hkl.Types.Parameter
       ( Parameter(..)
       , Range(..)
       , copyParameter
       , unit
       ) where

import Control.Monad (void)
import Foreign (nullPtr, Ptr, ForeignPtr, newForeignPtr, FunPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.C ( CInt(..)
                 , CDouble(..)
                 )
import Foreign.C.String ( CString, peekCString )
import Foreign.Storable ( Storable
                        , alignment
                        , sizeOf
                        , peek
                        , poke
                        )

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

unit :: CInt
unit = 1

-- | Range

data Range
  = Range
    Double -- ^ minimum value
    Double -- ^ maximum value
  deriving (Show)

-- | Parameter

data Parameter
  = Parameter
    String -- ^ name
    Double -- ^ value
    Range -- ^ range
  deriving (Show)

instance Storable Parameter where
    alignment _ = #{alignment int}
    sizeOf _ = #{size int}
    peek ptr = alloca $ \pmin ->
               alloca $ \pmax -> do
                              cname <- c_hkl_parameter_name_get ptr
                              name <- peekCString cname
                              value <- c_hkl_parameter_value_get ptr unit
                              c_hkl_parameter_min_max_get ptr pmin pmax unit
                              min_ <- peek pmin
                              max_ <- peek pmax
                              return (Parameter name value (Range min_ max_))
    poke ptr (Parameter _name value (Range min_ max_)) = do
                              void $ c_hkl_parameter_value_set ptr (CDouble value) unit nullPtr
                              void $ c_hkl_parameter_min_max_set ptr (CDouble min_) (CDouble max_) unit nullPtr

copyParameter :: Ptr Parameter -> IO (ForeignPtr Parameter)
copyParameter p = newForeignPtr c_hkl_parameter_free =<< c_hkl_parameter_new_copy p

foreign import ccall unsafe "hkl.h hkl_parameter_name_get"
  c_hkl_parameter_name_get:: Ptr Parameter -> IO CString

foreign import ccall unsafe "hkl.h hkl_parameter_value_get"
  c_hkl_parameter_value_get:: Ptr Parameter -> CInt -> IO Double

foreign import ccall unsafe "hkl.h hkl_parameter_min_max_get"
  c_hkl_parameter_min_max_get :: Ptr Parameter -> Ptr Double -> Ptr Double -> CInt -> IO ()

foreign import ccall unsafe "hkl.h &hkl_parameter_free"
  c_hkl_parameter_free :: FunPtr (Ptr Parameter -> IO ())

foreign import ccall unsafe "hkl.h hkl_parameter_new_copy"
  c_hkl_parameter_new_copy:: Ptr Parameter -> IO (Ptr Parameter)

foreign import ccall unsafe "hkl.h hkl_parameter_value_set"
  c_hkl_parameter_value_set:: Ptr Parameter -> CDouble -> CInt -> Ptr () -> IO (CInt)

foreign import ccall unsafe "hkl.h hkl_parameter_min_max_set"
  c_hkl_parameter_min_max_set :: Ptr Parameter -> CDouble -> CDouble -> CInt -> Ptr () -> IO (CInt)
