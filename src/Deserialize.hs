module Deserialize (deserialize) where

import Data.Bits ((.<<.), (.|.))
import Data.Functor ((<&>))
import Bits (u32)
import Data.ByteString.Internal (w2c)
import Data.Word (Word8)
import GHC.Float (castWord32ToFloat)
import Type (Type(..))
import Utils (Safe(..))
import VM (Any(..))

toAnyAndBytes :: Show a => Type -> (a, [Word8]) -> (Any, [Word8])
toAnyAndBytes t (val, bytes) = (Any (t, val), bytes)

deserialize :: Type -> [Word8] -> Safe (Any, [Word8])
deserialize T_Bool bytes = deserializeBool bytes <&> toAnyAndBytes T_Bool
deserialize T_Char bytes = deserializeChar bytes <&> toAnyAndBytes T_Char
deserialize T_Int bytes = deserializeInt bytes <&> toAnyAndBytes T_Int
deserialize T_NULL bytes = deserializeTypeNull bytes <&> toAnyAndBytes T_NULL
deserialize T_Float bytes = deserializeFloat bytes <&> toAnyAndBytes T_Float
deserialize a _ = Error ("Deserializing " ++ show a ++ " isn't implemented for now !")

deserializeBool :: [Word8] -> Safe (Bool, [Word8])
deserializeBool (0x00 : xs) = Value (False, xs)
deserializeBool (0x01 : xs) = Value (True, xs)
deserializeBool (a : _) = Error ("Got unexpected byte " ++ show a ++ " while trying to deserialize a boolean !")
deserializeBool [] = Error "Cannot deserialize a boolean, no byte to read !"

deserializeChar :: [Word8] -> Safe (Char, [Word8])
deserializeChar (x : xs) = Value (w2c x, xs)
deserializeChar [] = Error "Cannot deserialize a char, no byte to read !"

deserializeInt :: [Word8] -> Safe (Int, [Word8])
deserializeInt (b1 : b2 : b3 : b4 : bytes) = Value (fromIntegral int, bytes)
    where int = (u32 b1 .<<. 24) .|. (u32 b2 .<<. 16) .|. (u32 b3 .<<. 8)  .|. u32 b4
deserializeInt bytes = Error ("Cannot deserialize an int, less than 4 bytes to read (got " ++ show (length bytes) ++ " bytes) !")

deserializeTypeNull :: [Word8] -> Safe (Int, [Word8])
deserializeTypeNull (0x09 : xs) = Value (0x00, xs) -- 0x00 is a dummy value
deserializeTypeNull _ = Error "Cannot deserialize NULL type, no byte to read !"

deserializeFloat :: [Word8] -> Safe (Float, [Word8])
deserializeFloat (b1 : b2 : b3 : b4 : bytes) = Value (castWord32ToFloat float, bytes)
    where float = (u32 b1 .<<. 24) .|. (u32 b2 .<<. 16) .|. (u32 b3 .<<. 8)  .|. u32 b4
deserializeFloat bytes = Error ("Cannot deserialize a float, less than 4 bytes to read (got " ++ show (length bytes) ++ " bytes) !")


