{-# LANGUAGE CPP                         #-}
{-# LANGUAGE ForeignFunctionInterface    #-}
{-# LANGUAGE EmptyDataDecls              #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE RecordWildCards            #-}

module Hkl.Tango.DeviceProxy (
       deviceproxy
       , DeviceProxy ) where

import Control.Exception

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#include "tango.h"

data DeviceProxy = DeviceProxy

foreign import ccall "_ZN5Tango11DeviceProxyC1EPKcPN5CORBA3ORBE" deviceproxy_DeviceProxy :: (Ptr DeviceProxy) -> CString -> Ptr a -> IO ()

class New a where
    new :: IO (Ptr a)

instance Storable DeviceProxy where
  sizeOf _ = #{size Tango::DeviceProxy}

deviceproxy :: String -> IO (Ptr DeviceProxy)
deviceproxy d = do
  device <- newCString d
  dev <- malloc :: IO (Ptr DeviceProxy)
  deviceproxy_DeviceProxy dev device nullPtr
  return dev

main :: IO ()
main = do
  diffractometer <- catch (deviceproxy "toto")
                          (\e -> do let err = show (e :: IOException)
                              hPutStr stderr ("Warning: Couldn't open " ++ f ++ ": " ++ err)
                              return "")
  return ()
