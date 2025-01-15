{-# LANGUAGE NumericUnderscores #-}
module Bits where

import Data.Bits ((.<<.), (.>>.), (.&.))
import Data.Word (Word32, Word8)

splitWord32 :: Word32 -> [Word8]
splitWord32 w32 =   map fromIntegral [w32 .>>. 24
                    , (w32 .&. 0xFF_00_00) .>>. 16
                    , (w32 .&. 0xFF_00) .>>. 8
                    , w32 .&. 0xFF]

u32 :: Integral a => a -> Word32
u32 = fromIntegral

nthBit :: Bool -> Int -> Word8
nthBit bool = (((if bool then 0x01 else 0x00) :: Word8) .<<.)

setBit :: Int -> Bool -> [Word8] -> [Word8]
setBit _ _ [] = []
setBit nth bit (x : xs)
    | nth < 0 = error $ "Bit index must be positive, but got " ++ show nth ++ " !"
    | nth < 8 = (x .&. (nthBit bit nth)) : xs
    | otherwise = x : setBit (nth - 8) bit xs