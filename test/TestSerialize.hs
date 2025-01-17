{-# LANGUAGE NumericUnderscores #-}

module TestSerialize (testSerialize) where

import Data.ByteString.Internal (c2w)
import GHC.Float (castFloatToWord32)
import Test.HUnit

import Bits (splitWord32)
import Type
import Serialize
import UtilsForTests

testSerializeBool1 :: Test
testSerializeBool1 = myAssertEqual "True" [[0x01]] [serializeBool True]

testSerializeBool2 :: Test
testSerializeBool2 = myAssertEqual "False" [[0x00]] [serializeBool False]

testSerializeChar :: Test
testSerializeChar = myAssertEqual "Char" [c2w 'a'] (serializeChar 'a')

testSerializeInt :: Test
testSerializeInt = myAssertEqual "Normal int (1)" [[0x00] ++ [0x00] ++ [0x00] ++ [0x01]] [serializeInt 1]

testSerializeFloat :: Test
testSerializeFloat = myAssertEqual "Normal float (3.14)" [splitWord32 (castFloatToWord32 3.14)] [serializeFloat 3.14]

testSerializeTuple :: Test
testSerializeTuple = myAssertEqual "Normal tuple (int, char)" (serializeInt (1 :: Int) ++ serializeChar 'a') (serializeTuple ((1 :: Int), ('a' :: Char)))

testSerializeList :: Test
testSerializeList = myAssertEqual "int list [0,7,-78]" (serializeUInt 3 ++ serializeInt 0 ++ serializeInt 7 ++ serializeInt (-78)) (serializeList [(0 :: Int),(7 :: Int),((-78) :: Int)])

testSerializeValues :: Test
testSerializeValues = TestList [
    TestLabel "True" testSerializeBool1,
    TestLabel "False" testSerializeBool2,
    TestLabel "Char" testSerializeChar,
    TestLabel "Normal int (1)" testSerializeInt,
    TestLabel "Normal float (3.14)" testSerializeFloat,
    TestLabel "Normal tuple (int, char)" testSerializeTuple,
    TestLabel "int list [1,1,1]" testSerializeList
    ]


testSerializeTypeBool :: Test
testSerializeTypeBool = myAssertEqual "bool" [[0x01]] [serializeTypeBool]

testSerializeTypeInt :: Test
testSerializeTypeInt = myAssertEqual "int" [[0x02]] [serializeTypeInt]

testSerializeTypeUInt :: Test
testSerializeTypeUInt = myAssertEqual "uint" [[0x03]] [serializeTypeUInt]

testSerializeTypeFloat :: Test
testSerializeTypeFloat = myAssertEqual "float" [[0x04]] [serializeTypeFloat]

testSerializeTypeChar :: Test
testSerializeTypeChar = myAssertEqual "char" [[0x05]] [serializeTypeChar]

testSerializeTypeTuple :: Test
testSerializeTypeTuple = myAssertEqual "(int, char)" [[0x06] ++ [0x02] ++ [0x05]] [serializeTypeTuple (T_Int, T_Char)]

testSerializeTypeList :: Test
testSerializeTypeList = myAssertEqual "[char]" [[0x07] ++ [0x05]] [serializeTypeList T_Char]

testSerializeTypeEmptyList :: Test
testSerializeTypeEmptyList = myAssertEqual "[]" [[0x07] ++ [0x02]] [serializeTypeEmptyList]

testSerializeTypeCombination :: Test
testSerializeTypeCombination = myAssertEqual "[int, char, bool]" [[0x08] ++ [0x02] ++ [0x05] ++ [0x01]] [serializeTypeCombination [T_Int, T_Char, T_Bool]]

testSerializeTypeNull :: Test
testSerializeTypeNull = myAssertEqual "null" [[0x09]] [serializeTypeNull]

testSerializeTypes :: Test
testSerializeTypes = TestList [
    TestLabel "bool" testSerializeTypeBool,
    TestLabel "int" testSerializeTypeInt,
    TestLabel "uint" testSerializeTypeUInt,
    TestLabel "float" testSerializeTypeFloat,
    TestLabel "char" testSerializeTypeChar,
    TestLabel "[char]" testSerializeTypeList,
    TestLabel "[]" testSerializeTypeEmptyList,
    TestLabel "[int, char, bool]" testSerializeTypeCombination,
    TestLabel "null" testSerializeTypeNull
    ]

testSerialize :: Test
testSerialize = TestList [ testSerializeValues, testSerializeTypes ]