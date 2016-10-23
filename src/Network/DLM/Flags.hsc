module Network.DLM.Flags (
      Flag
    , noQueue, cancel, convert, valblk, quecvt, ivvalblk, convdeadlk
    , persistent, nodlckwt, nodlckblk, expedite, noqueuebast, headque, noorder
    , orphan, altpr, altcw, forceunlock, timeout, wait
    , FlagSet(..)
    , mkFlagSet
    ) where

#include <stdint.h>
#include <stdlib.h>
#include <libdlm.h>

import Data.Bits ((.|.))
import Data.Word (Word32)

newtype Flag = Flag { getFlag :: #{type uint32_t} }
    deriving (Show, Eq)

#{enum Flag, Flag
 , noQueue = LKF_NOQUEUE
 , cancel = LKF_CANCEL
 , convert = LKF_CONVERT
 , valblk = LKF_VALBLK
 , quecvt = LKF_QUECVT
 , ivvalblk = LKF_IVVALBLK
 , convdeadlk = LKF_CONVDEADLK
 , persistent = LKF_PERSISTENT
 , nodlckwt = LKF_NODLCKWT
 , nodlckblk = LKF_NODLCKBLK
 , expedite = LKF_EXPEDITE
 , noqueuebast = LKF_NOQUEUEBAST
 , headque = LKF_HEADQUE
 , noorder = LKF_NOORDER
 , orphan = LKF_ORPHAN
 , altpr = LKF_ALTPR
 , altcw = LKF_ALTCW
 , forceunlock = LKF_FORCEUNLOCK
 , timeout = LKF_TIMEOUT
 , wait = LKF_WAIT
 }

newtype FlagSet = FlagSet { getFlagSet :: #{type uint32_t} }
    deriving (Show, Eq)

mkFlagSet :: [Flag] -> FlagSet
mkFlagSet = FlagSet . foldr ((.|.) . getFlag) 0
