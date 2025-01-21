module Hex (Word32, showHex32) where

import Data.Word (Word32)

showHex' :: Int -> Word32 -> String
showHex' nbytes u32
    | nbytes == 0 = ""
    | u32 < 0x10 = ["0123456789ABCDEF" !! fromIntegral u32]
    | otherwise = showHex' 1 (rem u32 0x10) ++ showHex' (nbytes - 1) (div u32 0x10)

showHex32' :: Word32 -> String
showHex32' = showHex' 4

showHex32 :: Word32 -> String
showHex32 = ("0x" ++) . reverse . showHex32'