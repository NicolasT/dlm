{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Network.DLM
    ( kernelVersion
    , libraryVersion
    , Mode(..)
    , Flag(..)
    , lock
    , unlock
    , eventLoop
    ) where

#include <stdint.h>
#include <stdlib.h>
#include <libdlm.h>

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (onException, throwIO)
import Control.Monad (forever)
import Data.Word (Word32)
import Foreign.C
    (Errno(..), errnoToIOError, throwErrnoIfMinus1Retry_, withCStringLen)
import Foreign.Marshal (alloca, free, malloc)
import Foreign.Ptr (FunPtr, Ptr, nullFunPtr, nullPtr)
import Foreign.Storable (Storable(..), peek)
import GHC.Conc.IO (threadWaitRead)
import System.IO.Error (mkIOError, userErrorType)
import System.Posix.Types (Fd(..))

import Network.DLM.Bindings
    ( dlm_dispatch, dlm_get_fd, dlm_kernel_version, dlm_library_version
    , dlm_lock, dlm_unlock)
import Network.DLM.Lksb (Lksb(..))
import qualified Network.DLM.Flags as Flags
import qualified Network.DLM.Mode as Mode

kernelVersion :: IO (Word, Word, Word)
kernelVersion = alloca $ \ptr1 -> alloca $ \ptr2 -> alloca $ \ptr3 -> do
    dlm_kernel_version ptr1 ptr2 ptr3 >>= \case
        -1 -> throwIO $ mkIOError userErrorType "dlm_kernel_version" Nothing Nothing
        _ -> (,,) <$> fromIntegral `fmap` peek ptr1
                  <*> fromIntegral `fmap` peek ptr2
                  <*> fromIntegral `fmap` peek ptr3

libraryVersion :: IO (Word, Word, Word)
libraryVersion = alloca $ \ptr1 -> alloca $ \ptr2 -> alloca $ \ptr3 -> do
    dlm_library_version ptr1 ptr2 ptr3
    (,,) <$> fromIntegral `fmap` peek ptr1
         <*> fromIntegral `fmap` peek ptr2
         <*> fromIntegral `fmap` peek ptr3


eventLoop :: IO a
eventLoop = do
    fdi <- dlm_get_fd
    let fd = Fd (fromIntegral fdi)

    forever $ do
        threadWaitRead fd
        throwErrnoIfMinus1Retry_ "dlm_dispatch" $ dlm_dispatch fdi

foreign import ccall "wrapper"
    makeAstWrapper :: (Ptr Lksb  -> IO ()) -> IO (FunPtr (Ptr Lksb -> IO ()))

newtype LockID = LockID { getLockID ::#{type uint32_t} }
    deriving (Show, Eq)

astHandler :: MVar (Either Errno LockID) -> Ptr Lksb -> IO ()
astHandler slot lksbPtr = do
    lksb <- peek lksbPtr
    free lksbPtr
    putMVar slot $ case status lksb of
        0 -> Right $ LockID $ lkid lksb
        e -> Left $ Errno $ fromIntegral e

data Lock = Lock { lockID :: LockID
                 , lockSlot :: MVar (Either Errno LockID)
                 }

instance Show Lock where
    show = show . lockID

lock :: Mode
     -> [Flag]
     -> String
     -> IO Lock
lock mode flags name =
    withCStringLen name $ \(namePtr, nameLen) -> do
        slot <- newEmptyMVar
        lksbPtr <- malloc

        astaddr <- makeAstWrapper $ astHandler slot

        flip onException (free lksbPtr) $
            throwErrnoIfMinus1Retry_ "dlm_lock" $
                dlm_lock (mapMode mode)
                         lksbPtr
                         (Flags.mkFlagSet $ map mapFlag flags)
                         namePtr
                         (fromIntegral nameLen)
                         0
                         astaddr
                         lksbPtr
                         nullFunPtr
                         nullPtr

        takeMVar slot >>= \case
            Left e -> throwIO $ errnoToIOError "dlm_lock" e Nothing Nothing
            Right l -> pure $ Lock { lockID = l
                                   , lockSlot = slot
                                   }


unlock :: Lock -> [Flag] -> IO ()
unlock Lock{..} flags = do
    lksbPtr <- malloc

    flip onException (free lksbPtr) $
        throwErrnoIfMinus1Retry_ "dlm_unlock" $
            dlm_unlock (getLockID lockID)
                       (Flags.mkFlagSet $ map mapFlag flags)
                       lksbPtr
                       lksbPtr

    takeMVar lockSlot >>= \case
        Left e@(Errno errno) -> case errno of
            #{const EUNLOCK} -> pure ()
            _ -> throwIO $ errnoToIOError "dlm_unlock" e Nothing Nothing
        Right _ -> throwIO $ userError "dlm_unlock: status 0, expected EUNLOCK"


data Mode = Null
          | ConcurrentRead
          | ConcurrentWrite
          | ProtectedRead
          | ProtectedWrite
          | Exclusive
    deriving (Show, Eq, Read)

mapMode :: Mode -> Mode.Mode
mapMode = \case
    Null -> Mode.nl
    ConcurrentRead -> Mode.cr
    ConcurrentWrite -> Mode.cw
    ProtectedRead -> Mode.pr
    ProtectedWrite -> Mode.pw
    Exclusive -> Mode.ex

-- TODO
data Flag = NoQueue
    deriving (Show, Eq, Read)

mapFlag :: Flag -> Flags.Flag
mapFlag = \case
    NoQueue -> Flags.noQueue
