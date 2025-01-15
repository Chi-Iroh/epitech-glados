{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
module Deserialize where

import Data.Bits (complement, shiftR, (.&.))
import Bits (splitWord32, setBit)
import Data.ByteString.Internal (c2w)
import Data.Word (Word8)
import GHC.Float (castFloatToWord32)
import Limits (checkInt, checkUInt, checkFloat)
import Type (Type(..))
import Data.Char(ord)


newtype Combination = C_Combination [Type]
newtype Null = C_Null (Maybe Int)
newtype EmptyList = T_EmptyList [Null]

class Deserializable a where
    deserialize :: [Word8] -> a

class DeserializableType [Word8] where
    deserializeType :: [Word8] -> a

instance Deserializable Bool where
    deserialize b = deserializeBool b

instance Deserializeble Char where
    deserialize c = deserializeChar c

instance Deserializable Int where
    deserialize i = deserializeInt i

instance Deserializable Float where
    deserialize f = deserializeInt f

instance DeserializableType Null where
    deserialize _ = deserializeTypeNull

deserializeBool :: [Word8] -> Bool
deserializeBool [0x00] = False
deserializeBool [0x01] = True

deserializeChar :: [Word8] -> Char
deserializeChar [w] = chr (fromIntegral w)

deserializeUInt :: [Word8] -> Int
deserializeUInt uint
    | not (checkUInt (deserializeInt' uint)) = error "Negative uint !"
    | otherwise = deserializeInt' uint

deserializeInt :: [Word8] -> Int
deserializeInt int
    | not (checkInt (deserializeInt' int)) = error "Out of range int !"
    | otherwise = deserializeInt' int

deserializeInt' :: [Word8] -> Int
deserializeInt' [b1, b2, b3, b4] =
    (fromIntegral w1 `shiftL` 24) .|.
    (fromIntegral w2 `shiftL` 16) .|.
    (fromIntegral w3 `shiftL` 8)  .|.
    fromIntegral w4
deserializeInt' _ = error "Not a number"

deserializeFloat :: [Word8] -> Float
deserializeFloat float
    | not (checkFloat (deserializeFloat' float)) = error "Out of range float !"
    | otherwise = deserializeFloat' float

deserializeFloat' :: [Word8] -> Float
deserializeFloat' [w1, w2, w3, w4] =
    wordToFloat $
    (fromIntegral w1 `shiftL` 24) .|.
    (fromIntegral w2 `shiftL` 16) .|.
    (fromIntegral w3 `shiftL` 8)  .|.
    fromIntegral w4

combineWord32 :: [Word8] -> Word32
combineWord32 [w1, w2, w3, w4] =
    (fromIntegral w1 `shiftL` 24) .|.
    (fromIntegral w2 `shiftL` 16) .|.
    (fromIntegral w3 `shiftL` 8)  .|.
    fromIntegral w4
combineWord32 _ = error "Invalid input for combining Word32"

deserializeInt :: [Word8] -> Int
deserializeInt bytes
    | all (== 0xFF) bytes = -1
    | testBit combined 31 =
        let absValue = complement (fromEnum (clearSignBit combined)) + 1
        in -absValue
    | otherwise = fromEnum combined
  where
    combined = combineWord32 bytes
    clearSignBit w = w `clearBit` 31






