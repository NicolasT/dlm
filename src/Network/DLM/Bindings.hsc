{-# LANGUAGE RecordWildCards #-}

module Network.DLM.Bindings (
      dlm_dispatch
    , dlm_get_fd
    , dlm_kernel_version
    , dlm_library_version
    , dlm_lock
    , dlm_unlock
    ) where

#include <stdint.h>
#include <stdlib.h>
#include <libdlm.h>

import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.Ptr (FunPtr, Ptr)

import Network.DLM.Flags (FlagSet(..))
import Network.DLM.Lksb (Lksb)
import Network.DLM.Mode (Mode(..))

foreign import ccall unsafe "libdlm.h dlm_kernel_version"
    dlm_kernel_version :: Ptr (#type uint32_t)
                       -> Ptr (#type uint32_t)
                       -> Ptr (#type uint32_t)
                       -> IO (#type int)

foreign import ccall unsafe "libdlm.h dlm_library_version"
    dlm_library_version :: Ptr (#type uint32_t)
                        -> Ptr (#type uint32_t)
                        -> Ptr (#type uint32_t)
                        -> IO ()

foreign import ccall unsafe "libdlm.h dlm_get_fd"
    dlm_get_fd :: IO (#type int)

foreign import ccall safe "libdlm.h dlm_dispatch"
    dlm_dispatch :: (#type int)
                 -> IO (#{type int})

foreign import ccall safe "libdlm.h dlm_lock"
    dlm_lock :: Mode -- mode
             -> Ptr Lksb -- lksb
             -> FlagSet -- flags
             -> Ptr name -- name
             -> #{type unsigned int} -- namelen
             -> #{type uint32_t} -- parent, ununsed
             -> FunPtr (Ptr astarg -> IO ()) -- astaddr
             -> Ptr astarg -- astarg
             -> FunPtr (Ptr astarg -> IO ()) -- bastaddr
             -> Ptr range -- range, unused
             -> IO (#{type int})

foreign import ccall safe "libdlm.h dlm_unlock"
    dlm_unlock :: #{type uint32_t} -- lkid
               -> FlagSet -- flags
               -> Ptr Lksb -- lksb
               -> Ptr astarg -- astarg
               -> IO (#{type int})
