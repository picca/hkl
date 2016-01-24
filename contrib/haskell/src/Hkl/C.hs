{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hkl.C
       ( factoryGetAll
       , factoryNameGet
       ) where

import Foreign (Ptr, peek, poke)
import Foreign.C.Types
import Foreign.C.String (CString, peekCString)
import Foreign.Marshal.Alloc (alloca, malloc, free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (Storable)

-- data Factory
newtype Factory = Factory (Ptr Factory) deriving (Show, Storable)

factoryGetAll :: IO [Factory]
factoryGetAll = alloca $ \ptr -> do
                 factories <- c_hkl_factory_get_all ptr
                 n <- peek ptr
                 peekArray n factories

foreign import ccall unsafe "hkl.h hkl_factory_get_all"
  c_hkl_factory_get_all :: Ptr Int
                        -> IO (Ptr Factory)

factoryNameGet :: Factory
                  -> IO String
factoryNameGet factory = do
  name <- c_hkl_factory_name_get factory
  peekCString name

foreign import ccall unsafe "hkl.h hkl_factory_name_get"
  c_hkl_factory_name_get :: Factory
                         -> IO CString
