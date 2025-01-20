{-# LANGUAGE TupleSections #-}

module Deserialize (deserialize, deserializeTypeAndValue, deserializeType, deserializeList, deserializeInt, addBytesLen) where

import Data.Bits ((.<<.), (.|.))
import Data.Functor ((<&>))
import Bits (u32)
import Data.ByteString.Internal (w2c)
import Data.Word (Word8)
import GHC.Float (castWord32ToFloat)
import Serialize (Serializable)
import Type (Type(..))
import Utils (Safe(..))
import VMData (Any(..))

toAnyAndBytes :: (Serializable a, Show a) => Type -> (a, Int, [Word8]) -> (Any, Int, [Word8])
toAnyAndBytes t (val, len, bytes) = (Any (t, val), len, bytes)

deserialize :: Type -> [Word8] -> Safe (Any, Int, [Word8])
deserialize T_Bool bytes = deserializeBool bytes <&> toAnyAndBytes T_Bool
deserialize T_Char bytes = deserializeChar bytes <&> toAnyAndBytes T_Char
deserialize T_Int bytes = deserializeInt bytes <&> toAnyAndBytes T_Int
deserialize T_NULL bytes = deserializeTypeNull bytes <&> toAnyAndBytes T_NULL
deserialize T_Float bytes = deserializeFloat bytes <&> toAnyAndBytes T_Float
deserialize a _ = Error ("Deserializing " ++ show a ++ " isn't implemented for now !")

deserializeBool :: [Word8] -> Safe (Bool, Int, [Word8])
deserializeBool (0x00 : xs) = Value (False, 1, xs)
deserializeBool (0x01 : xs) = Value (True, 1, xs)
deserializeBool (a : _) = Error ("Got unexpected byte " ++ show a ++ " while trying to deserialize a boolean !")
deserializeBool [] = Error "Cannot deserialize a boolean, no byte to read !"

deserializeChar :: [Word8] -> Safe (Char, Int, [Word8])
deserializeChar (x : xs) = Value (w2c x, 1, xs)
deserializeChar [] = Error "Cannot deserialize a char, no byte to read !"

deserializeInt :: [Word8] -> Safe (Int, Int, [Word8])
deserializeInt (b1 : b2 : b3 : b4 : bytes) = Value (fromIntegral int, 4, bytes)
    where int = (u32 b1 .<<. 24) .|. (u32 b2 .<<. 16) .|. (u32 b3 .<<. 8)  .|. u32 b4
deserializeInt bytes = Error ("Cannot deserialize an int, less than 4 bytes to read (got " ++ show (length bytes) ++ " bytes) !")

deserializeTypeNull :: [Word8] -> Safe (Int, Int, [Word8])
deserializeTypeNull (0x09 : xs) = Value (0x00, 1, xs) -- 0x00 is a dummy value
deserializeTypeNull _ = Error "Cannot deserialize NULL type, no byte to read !"

deserializeFloat :: [Word8] -> Safe (Float, Int, [Word8])
deserializeFloat (b1 : b2 : b3 : b4 : bytes) = Value (castWord32ToFloat float, 4, bytes)
    where float = (u32 b1 .<<. 24) .|. (u32 b2 .<<. 16) .|. (u32 b3 .<<. 8)  .|. u32 b4
deserializeFloat bytes = Error ("Cannot deserialize a float, less than 4 bytes to read (got " ++ show (length bytes) ++ " bytes) !")

deserializeTypeCombinationTypes :: Int -> [Word8] -> Safe ([Type], Int, [Word8])
deserializeTypeCombinationTypes 0 list = deserializeType list >>=(\(_type, size, rest) -> Value ([_type], size, rest))
deserializeTypeCombinationTypes x [] = Error "Not enough element to deserialize combination type"
deserializeTypeCombinationTypes x list = deserializeType list >>=(\(_type, size, rest) -> deserializeTypeCombinationTypes (x - 1) rest >>=(\(_list, _size, _rest') -> Value (_type:_list, size + _size, _rest')))

deserializeType :: [Word8] -> Safe (Type, Int, [Word8])
deserializeType (0x01 : xs) = Value (T_Bool, 1, xs)
deserializeType (0x02 : xs) = Value (T_Int, 1, xs)
deserializeType (0x03 : xs) = Value (T_UInt, 1, xs)
deserializeType (0x04 : xs) = Value (T_Float, 1, xs)
deserializeType (0x05 : xs) = Value (T_Char, 1, xs)
deserializeType (0x06 : xs) = deserializeType xs >>=(\(type1, size1, rest) -> deserializeType rest >>=(\(type2, size2, rest') -> Value (T_Tuple (type1, type2), 1 + size1 + size2, rest')))
deserializeType (0x07 : xs) = deserializeType xs >>=(\(_type, _size, rest) -> Value (T_List _type, _size + 1, rest))
deserializeType (0x08 : xs) = deserializeInt xs >>=(\(_value, len, rest)-> deserializeTypeCombinationTypes _value rest >>= (\(_type, _size, _rest) -> Value (T_Combination _type, _size + 1 + len, _rest)))
deserializeType bytes = Value (T_NULL, 1, bytes)

deserializeList :: Type -> Int -> [Word8] -> [Any] -> Safe ([Any], Int)
deserializeList _ len (0x00:_) list = Value (list, len)
deserializeList _ _ [] _ = Error "No end bytes in list"
deserializeList a len bytes list = deserialize a bytes >>= (\(member, len', rest) -> deserializeList a (len + len') rest $ list ++ [member])

deserializeTypeAndValue :: [Word8] -> Safe (Any, Int)
deserializeTypeAndValue bytes = deserializeType bytes >>= (\(_type, len, rest) -> deserialize _type rest >>= \(a, len', rest') -> Value (Any (_type, a), len + len'))

addBytesLen :: Int -> (Any, Int) -> (Any, Int)
addBytesLen n (val, len) = (val, len + n)