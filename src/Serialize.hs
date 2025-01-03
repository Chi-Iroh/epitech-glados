{-# LANGUAGE NumericUnderscores #-}
module Serialize where

import Data.Bits (complement)
import Data.ByteString.Internal (c2w)
import Data.Word (Word8)
import Bits (splitWord32, setBit)
import IntLimits (checkInt, checkUInt)
import Type (Type(..))

serializeBool :: Bool -> [Word8]
serializeBool bool = [if bool then 0x01 else 0x00]

serializeChar :: Char -> [Word8]
serializeChar char = [c2w char]

serializeUInt :: Int -> [Word8]
serializeUInt uint
    | not (checkUInt uint) = error "Negative uint !"
    | otherwise = splitWord32 (toEnum uint)

serializeInt' :: Int -> [Word8]
serializeInt' int
    | int >= 0 = splitWord32 (toEnum int)
    | int == (-1) = replicate 4 0xFF
    | otherwise = setBit 0 True ((serializeInt' . complement . abs) (int - 1))

serializeInt :: Int -> [Word8]
serializeInt int
    | not (checkInt int) = error "Out of range int !"
    | otherwise = serializeInt' int

serializeType :: Type -> [Word8]
serializeType = const [0x00]