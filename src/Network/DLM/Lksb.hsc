{-# LANGUAGE RecordWildCards #-}

module Network.DLM.Lksb (
      Lksb(..)
    ) where

#include <stdint.h>
#include <stdlib.h>
#include <libdlm.h>

import Data.Int (Int8, Int32)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

data Lksb = Lksb { status :: #{type int}
                 , lkid :: #{type uint32_t}
                 , flags :: #{type char}
                 , lvbptr :: Ptr (#type char)
                 }

instance Storable Lksb where
    sizeOf _ = #{size struct dlm_lksb}
    alignment _ = alignment (undefined :: #{type int})

    poke p Lksb{..} = do
        #{poke struct dlm_lksb, sb_status} p status
        #{poke struct dlm_lksb, sb_lkid} p lkid
        #{poke struct dlm_lksb, sb_flags} p flags
        #{poke struct dlm_lksb, sb_lvbptr} p lvbptr

    peek p = Lksb <$> (#{peek struct dlm_lksb, sb_status} p)
                  <*> (#{peek struct dlm_lksb, sb_lkid} p)
                  <*> (#{peek struct dlm_lksb, sb_flags} p)
                  <*> (#{peek struct dlm_lksb, sb_lvbptr} p)
