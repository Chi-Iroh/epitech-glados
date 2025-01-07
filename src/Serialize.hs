{-# LANGUAGE NumericUnderscores #-}
module Serialize where

import Data.Bits (complement, setBit, shiftR, (.&.))
import Bits (splitWord32)
import Data.ByteString.Internal (c2w)
import Data.Word (Word8)
import Limits (checkInt, checkUInt, checkFloat)
import Type (Type(..))

import Data.Char(ord)

newtype Combination = C_Combination [Type]
newtype Null = C_Null (Maybe Int)
newtype Tuple = T_Tuple (Type, Type)
newtype TypeList = T_TypeList [Type]
newtype EmptyList = T_EmptyList [Null]

class Serializable a where
    serialize :: a -> [Word8]

class SerializableType a where
    serializeType :: a -> [Word8]

instance Serializable Bool where
    serialize b = serializeBool b

instance Serializable Int where
    serialize n = serializeInt n

instance Serializable Float where
    serialize f = serializeFloat f

instance Serializable Char where
    serialize c = serializeChar c

instance Serializable Null where
    serialize _ = serializeTypeNull

instance Serializable Tuple where
    serialize t = serializeTuple t

serializeBool :: Bool -> [Word8]
serializeBool bool = [if bool then 0x01 else 0x00]

serializeChar :: Char -> [Word8]
serializeChar char = [c2w char]

serializeUInt :: Int -> [Word8]
serializeUInt uint
    | not (checkUInt uint) = error "Negative uint !"
    | otherwise = serializeInt' uint

serializeInt' :: Int -> [Word8]
serializeInt' int
    | int >= 0 = splitWord32 (toEnum int)
    | int == (-1) = replicate 4 0xFF
    | otherwise = setBit 0 True ((serializeInt' . complement . abs) (int - 1))

serializeInt :: Int -> [Word8]
serializeInt int
    | not (checkInt int) = error "Out of range int !"
    | otherwise = serializeInt' int

serializeFloat' :: Float -> [Word8]
serializeFloat' float = splitWord32 (splitFloatToWord32 float)

serializeFloat :: Float -> [Word8]
serializeFloat float
    | not (checkFloat float) = error "Out of range float !"
    | otherwise = serializeFloat' float

serializeTuple :: (Type, Type) -> [Word8]
serializeTuple (x, y) = serialize x ++ serialize y

serializeList :: [Type] -> [Word8]
serializeList (x:xs) = serialize x ++ serializeList xs
serializeList [x] = serialize x

serializeCombination :: [Type] -> [Word8]
serializeCombination list = serializeList list


instance SerializableType Bool where
    serializeType b = serializeTypeBool

instance SerializableType Int where
    serializeType n = serializeTypeInt

instance SerializableType Float where
    serializeType f = serializeTypeFloat

instance SerializableType Char where
    serializeType c = serializeTypeChar

instance SerializableType Null where
    serializeType _ = serializeTypeNull

instance SerializableType Tuple where
    serializeType t = serializeTypeTuple t

instance SerializableType TypeList where
    serializeType l = serializeTypeList l

instance SerializableType EmptyList where
    serializeType e = serializeTypeEmptyList

instance SerializableType Combination where
    serializeType c = serializeTypeCombination c

serializeTypeBool :: [Word8]
serializeTypeBool = [0x01]

serializeTypeInt :: [Word8]
serializeTypeInt = [0x02]

serializeTypeUInt :: [Word8]
serializeTypeUInt = [0x03]

serializeTypeFloat :: [Word8]
serializeTypeFloat = [0x04]

serializeTypeChar :: [Word8]
serializeTypeChar = [0x05]

serializeTypeTuple :: (Type, Type) -> [Word8]
serializeTypeTuple (x, y) = [0x06] ++ serializeType x ++ serializeType y

serializeTypeList :: Type -> [Word8]
serializeTypeList t = [0x07] ++ serializeType t

serializeTypeEmptyList :: [Word8]
serializeTypeEmptyList = [0x07] ++ serializeTypeInt

serializeTypeCombination' :: [Type] -> [Word8]
serializeTypeCombination' (x:xs) = [0x08] ++ serializeType x ++ serializeTypeCombination' xs
serializeTypeCombination' [t] = serializeType t

serializeTypeCombination :: [Type] -> [Word8]
serializeTypeCombination list = [0x08] ++ serializeTypeCombination' list

serializeTypeNull :: [Word8]
serializeTypeNull = [0x09]

serializeTypeGeneric :: Type -> [Word8]
serializeTypeGeneric t = serializeType t