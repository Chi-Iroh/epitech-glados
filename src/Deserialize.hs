{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
module Deserialize where

import Data.Bits ((.<<.), (.|.))
import Bits (u32)
import Data.ByteString.Internal (w2c)
import Data.Word (Word8)
import Type (Type(..))
import Utils (Safe(..))

-- class Deserializable a where
--     deserialize :: [Word8] -> a

-- class DeserializableType [Word8] where
--     deserializeType :: [Word8] -> a

-- instance Deserializable Bool where
--     deserialize b = deserializeBool b

-- instance Deserializable Char where
--     deserialize c = deserializeChar c

-- instance Deserializable Int where
--     deserialize i = deserializeInt i

-- instance DeserializableType Null where
--     deserialize _ = deserializeTypeNull

deserializeBool :: [Word8] -> Safe (Bool, [Word8])
deserializeBool (0x00 : xs) = Value (False, xs)
deserializeBool (0x01 : xs) = Value (True, xs)
deserializeBool (a : _) = Error ("Got unexpected byte " ++ show a ++ " while trying to deserialize a boolean !")

deserializeChar :: [Word8] -> Safe (Char, [Word8])
deserializeChar (x : xs) = Value (w2c x, xs)
deserializeChar [] = Error "Cannot deserialize a char, no byte to read !"

deserializeInt :: [Word8] -> Safe (Int, [Word8])
deserializeInt (b1 : b2 : b3 : b4 : bytes) = Value (fromIntegral int, bytes)
    where int = (u32 b1 .<<. 24) .|. (u32 b2 .<<. 16) .|. (u32 b3 .<<. 8)  .|. u32 b4
deserializeInt bytes = Error ("Cannot deserialize an int, less than 4 bytes to read (got " ++ show (length bytes) ++ " bytes) !")






