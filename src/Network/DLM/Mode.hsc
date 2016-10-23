module Network.DLM.Mode (
      Mode(..)
    , nl, cr, cw, pr, pw, ex
    ) where

#include <stdint.h>
#include <stdlib.h>
#include <libdlm.h>

import Data.Word (Word32)

newtype Mode = Mode { getMode :: #{type uint32_t} }
    deriving (Show, Eq)

#{enum Mode, Mode
 , nl = LKM_NLMODE
 , cr = LKM_CRMODE
 , cw = LKM_CWMODE
 , pr = LKM_PRMODE
 , pw = LKM_PWMODE
 , ex = LKM_EXMODE
 }
