module TestConverter (testConverter) where

import Type
import Test.HUnit
import Serialize

testSerializeBool1 :: Test
testSerializeBool1 = myAssertEqual "True" [[0x01]] [serializeBool True]

testSerializeBool2 :: Test
testSerializeBool2 = myAssertEqual "False" [[0x00]] [serializeBool False]

testSerializeChar :: Test
testSerializeChar = myAssertEqual "Char" [c2w 'a'] [serializeChar 'a']

testSerializeInt1 :: Test
testSerializeInt1 = myAssertEqual "Normal int (1)" [[0x01]] [serializeInt 1]

testSerializeInt2 :: Test
testSerializeInt2 = myAssertEqual "Out of range int" [error "Out of range int !"] [serializeInt 2_147_483_648]

testSerializeFloat1 :: Test
testSerializeFloat1 = myAssertEqual "Normal float (3.14)" [splitWord32 (castFloatToWord32 3.14)] [serializeFloat 3.14]

testSerializeFloat2 :: Test
testSerializeFloat2 = myAssertEqual "Out of range float" [error "Out of range float !"] [serializeFloat 3.4028237e39]

testSerializeTuple :: Test
testSerializeTuple = myAssertEqual "Normal tuple (int, char)" [serializeInt 1 ++ serializeChar 'a'] [serializeTuple (1, 'a')]

testSerializeList :: Test
testSerializeList = myAssertEqual "int list [1,1,1]" [[0x01], [0x01], [0x01]] [serializeList [1,1,1]]

testSerializeValues :: Test
testSerializeValues = TestList [
    TestLabel "True" testSerializeBool1,
    TestLabel "False" testSerializeBool2,
    TestLabel "Char" testSerializeChar,
    TestLabel "Normal int (1)" testSerializeInt1,
    TestLabel "Out of range int" testSerializeInt2,
    TestLabel "Normal float (3.14)" testSerializeFloat1,
    TestLabel "Out of range float" testSerializeFloat2,
    TestLabel "Normal tuple (int, char)" testSerializeTuple,
    TestLabel "int list [1,1,1]" testSerializeList
]


testSerializeTypeBool :: Test
testSerializeTypeBool = myAssertEqual "bool" [[0x01]] [serializeTypeBool]

testSerializeTypeInt :: Test
testSerializeTypeInt = myAssertEqual "int" [[0x02]] [serializeTypeInt]

testSerializeTypeUInt :: Test
testSerializeTypeUInt = myAssertEqual "uint" [[0x03]] [serializeUInt]

testSerializeTypeFloat :: Test
testSerializeTypeFloat = myAssertEqual "float" [[0x04]] [serializeTypeFloat]

testSerializeTypeChar :: Test
testSerializeTypeChar = myAssertEqual "char" [[0x05]] [serializeTypeChar]

testSerializeTypeTuple :: Test
testSerializeTypeTuple = myAssertEqual "(int, char)" [[0x06] ++ [0x02] ++ [0x05]] [serializeTypeTuple (T_Int, T_Char)]

testSerializeTypeList :: Test
testSerializeTypeList = myAssertEqual "[char]" [[0x07] ++ [0x05]]

testSerializeTypeEmptyList :: Test
testSerializeTypeEmptyList = myAssertEqual "[]" [[0x07] ++ [0x02]] [serializeTypeEmptyList]

testSerializeTypeCombination :: Test
testSerializeTypeCombination = myAssertEqual "[int, char, bool]" [[0x02] ++ [0x05] ++ [0x01]] [serializeTypeCombination [T_Int, T_Bool, T_Char]]

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