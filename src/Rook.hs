module Rook where

import Data.Word      ( Word8 )
import Data.Int       ( Int8 )
import Data.Bits.Pext ( pext )
import Data.Bits      ( (.&.)
                      , (.|.)
                      , xor
                      , shiftR
                      , shiftL
                      , complement )

f :: Word8 -> Word8
f i =  complement $ i `xor` (1 + (complement i))
