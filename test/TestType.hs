module TestType (testType) where

import Test.HUnit
import UtilsForTests
import Type
import Utils

-------------------------------------------------------------------------------

testEqType1 :: Test
testEqType1 = myAssertEqual "int == int" (True) (T_Int == T_Int)

testEqType2 :: Test
testEqType2 = myAssertEqual "uint == uint" (True) (T_UInt == T_UInt)

testEqType3 :: Test
testEqType3 = myAssertEqual "char == char" (True) (T_Char == T_Char)

testEqType4 :: Test
testEqType4 = myAssertEqual "float == float" (True) (T_Float == T_Float)

testEqType5 :: Test
testEqType5 = myAssertEqual "bool == bool" (True) (T_Bool == T_Bool)

testEqType6 :: Test
testEqType6 = myAssertEqual "(int, int) == (int, int)" (True) ((T_Tuple (T_Int, T_Int)) == (T_Tuple (T_Int, T_Int)))

testEqType7 :: Test
testEqType7 = myAssertEqual "[int] == [int]" (True) ((T_List T_Int) == (T_List T_Int))

testEqType8 :: Test
testEqType8 = myAssertEqual "[] == []" (True) (T_EmptyList == T_EmptyList)

testEqType9 :: Test
testEqType9 = myAssertEqual "string == string" (True) (T_String == T_String)

testEqType10 :: Test
testEqType10 = myAssertEqual "procedure == procedure" (True) (T_Procedure == T_Procedure)

testEqType11 :: Test
testEqType11 = myAssertEqual "<(int) => int> == <(int) => int>" (True) ((T_Function [T_Int] T_Int) == (T_Function [T_Int] T_Int))

testEqType12 :: Test
testEqType12 = myAssertEqual "int|uint|char == integer" (True) ((T_Combination [T_Int, T_UInt, T_Char]) == typeInteger)

testEqType13 :: Test
testEqType13 = myAssertEqual "int|uint|char|float == number" (True) ((T_Combination [T_Int, T_UInt, T_Char, T_Float]) == typeNumber)

testEqType14 :: Test
testEqType14 = myAssertEqual "int|uint|char|float|bool|{a, a}|[a]|procedure == any" (True) ((T_Combination [T_Int, T_UInt, T_Char, T_Float, T_Bool, T_Tuple (T_Template, T_Template), T_List T_Template, T_Procedure]) == typeAny)

testEqType15 :: Test
testEqType15 = myAssertEqual "NULL == NULL" (True) (T_NULL == T_NULL)

testEqType16 :: Test
testEqType16 = myAssertEqual "a == a" (True) (T_Template == T_Template)

testEqType17 :: Test
testEqType17 = myAssertEqual "type == type" (True) (T_Type == T_Type)

testEqType18 :: Test
testEqType18 = myAssertEqual "undefined == undefined" (True) (T_Undefined == T_Undefined)

testEqType19 :: Test
testEqType19 = myAssertEqual "int == uint" (False) (T_Int == T_UInt)

testEqType20 :: Test
testEqType20 = myAssertEqual "[char] == string" (True) ((T_List T_Char) == T_String)

testEqType21 :: Test
testEqType21 = myAssertEqual "string == [char]" (True) (T_String == (T_List T_Char))

testEqType22 :: Test
testEqType22 = myAssertEqual "int /= int" (False) (T_Int /= T_Int)

testEqType :: Test
testEqType = TestList [
    TestLabel "Type deriving Eq" testEqType1,
    TestLabel "Type deriving Eq" testEqType2,
    TestLabel "Type deriving Eq" testEqType3,
    TestLabel "Type deriving Eq" testEqType4,
    TestLabel "Type deriving Eq" testEqType5,
    TestLabel "Type deriving Eq" testEqType6,
    TestLabel "Type deriving Eq" testEqType7,
    TestLabel "Type deriving Eq" testEqType8,
    TestLabel "Type deriving Eq" testEqType9,
    TestLabel "Type deriving Eq" testEqType10,
    TestLabel "Type deriving Eq" testEqType11,
    TestLabel "Type deriving Eq" testEqType12,
    TestLabel "Type deriving Eq" testEqType13,
    TestLabel "Type deriving Eq" testEqType14,
    TestLabel "Type deriving Eq" testEqType15,
    TestLabel "Type deriving Eq" testEqType16,
    TestLabel "Type deriving Eq" testEqType17,
    TestLabel "Type deriving Eq" testEqType18,
    TestLabel "Type deriving Eq" testEqType19,
    TestLabel "Type deriving Eq" testEqType20,
    TestLabel "Type deriving Eq" testEqType21,
    TestLabel "Type deriving Eq" testEqType22
    ]

-------------------------------------------------------------------------------

testShowType1 :: Test
testShowType1 = myAssertEqual "show int" ("int") (show T_Int)

testShowType2 :: Test
testShowType2 = myAssertEqual "show uint" ("uint") (show T_UInt)

testShowType3 :: Test
testShowType3 = myAssertEqual "show char" ("char") (show T_Char)

testShowType4 :: Test
testShowType4 = myAssertEqual "show float" ("float") (show T_Float)

testShowType5 :: Test
testShowType5 = myAssertEqual "show bool" ("bool") (show T_Bool)

testShowType6 :: Test
testShowType6 = myAssertEqual "show {int, int}" ("{int, int}") (show $ T_Tuple (T_Int, T_Int))

testShowType7 :: Test
testShowType7 = myAssertEqual "show [int]" ("[int]") (show $ T_List T_Int)

testShowType8 :: Test
testShowType8 = myAssertEqual "show []" ("[]") (show T_EmptyList)

testShowType9 :: Test
testShowType9 = myAssertEqual "show string" ("string") (show T_String)

testShowType10 :: Test
testShowType10 = myAssertEqual "show procedure" ("procedure") (show T_Procedure)

testShowType11 :: Test
testShowType11 = myAssertEqual "show <(int) => int>" ("<(int) => int>") (show $ T_Function [T_Int] T_Int)

testShowType12 :: Test
testShowType12 = myAssertEqual "show int|uint|char" ("int|uint|char") (show $ typeInteger)

testShowType13 :: Test
testShowType13 = myAssertEqual "show NULL" ("NULL") (show T_NULL)

testShowType14 :: Test
testShowType14 = myAssertEqual "show a" ("a") (show T_Template)

testShowType15 :: Test
testShowType15 = myAssertEqual "show type" ("type") (show T_Type)

testShowType16 :: Test
testShowType16 = myAssertEqual "show undefined" ("undefined") (show T_Undefined)

testShowType17 :: Test
testShowType17 = myAssertEqual "showsPrec 10 int $ \"\"" ("int") (showsPrec 10 T_Int $ "")

testShowType18 :: Test
testShowType18 = myAssertEqual "showList [int, uint]" ("[int,uint]") (showList [T_Int, T_UInt] "")

testShowType :: Test
testShowType = TestList [
    TestLabel "Type deriving Show" testShowType1,
    TestLabel "Type deriving Show" testShowType2,
    TestLabel "Type deriving Show" testShowType3,
    TestLabel "Type deriving Show" testShowType4,
    TestLabel "Type deriving Show" testShowType5,
    TestLabel "Type deriving Show" testShowType6,
    TestLabel "Type deriving Show" testShowType7,
    TestLabel "Type deriving Show" testShowType8,
    TestLabel "Type deriving Show" testShowType9,
    TestLabel "Type deriving Show" testShowType10,
    TestLabel "Type deriving Show" testShowType11,
    TestLabel "Type deriving Show" testShowType12,
    TestLabel "Type deriving Show" testShowType13,
    TestLabel "Type deriving Show" testShowType14,
    TestLabel "Type deriving Show" testShowType15,
    TestLabel "Type deriving Show" testShowType16,
    TestLabel "Type deriving Show" testShowType17,
    TestLabel "Type deriving Show" testShowType18
    ]

-------------------------------------------------------------------------------

testVerifyTypeList1 :: Test
testVerifyTypeList1 = myAssertEqual "verifyTypeList []" (T_EmptyList) (verifyTypeList [])

testVerifyTypeList2 :: Test
testVerifyTypeList2 = myAssertEqual "verifyTypeList [int]" (T_List T_Int) (verifyTypeList [T_Int])

testVerifyTypeList3 :: Test
testVerifyTypeList3 = myAssertEqual "verifyTypeList [int, int, int]" (T_List T_Int) (verifyTypeList [T_Int, T_Int, T_Int])

testVerifyTypeList4 :: Test
testVerifyTypeList4 = myAssertEqual "verifyTypeList [int, uint, int]" (T_Undefined) (verifyTypeList [T_Int, T_UInt, T_Int])

testVerifyTypeList :: Test
testVerifyTypeList = TestList [
    TestLabel "verifyTypeList" testVerifyTypeList1,
    TestLabel "verifyTypeList" testVerifyTypeList2,
    TestLabel "verifyTypeList" testVerifyTypeList3,
    TestLabel "verifyTypeList" testVerifyTypeList4
    ]

-------------------------------------------------------------------------------

testCombinateTypes1 :: Test
testCombinateTypes1 = myAssertEqual "combinateTypes [Value int, Value uint, Value char]" (Value $ T_Combination [T_Int, T_UInt, T_Char]) (combinateTypes [Value T_Int, Value T_UInt, Value T_Char])

testCombinateTypes2 :: Test
testCombinateTypes2 = myAssertEqual "combinateTypes [Value int, Error test, Value char]" (Error "test") (combinateTypes [Value T_Int, Error "test", Value T_Char])

testCombinateTypes :: Test
testCombinateTypes = TestList [
    TestLabel "combinateTypes" testCombinateTypes1,
    TestLabel "combinateTypes" testCombinateTypes2
    ]

-------------------------------------------------------------------------------

testVerifyType1 :: Test
testVerifyType1 = myAssertEqual "verifyType [] int" (False) (verifyType T_EmptyList T_Int)

testVerifyType2 :: Test
testVerifyType2 = myAssertEqual "verifyType NULL int" (False) (verifyType T_NULL T_Int)

testVerifyType3 :: Test
testVerifyType3 = myAssertEqual "verifyType undefined int" (False) (verifyType T_Undefined T_Int)

testVerifyType4 :: Test
testVerifyType4 = myAssertEqual "verifyType | int" (False) (verifyType (T_Combination []) T_Int)

testVerifyType5 :: Test
testVerifyType5 = myAssertEqual "verifyType int undefined" (False) (verifyType T_Int T_Undefined)

testVerifyType6 :: Test
testVerifyType6 = myAssertEqual "verifyType int|uint |" (False) (verifyType (T_Combination [T_Int, T_UInt]) (T_Combination []))

testVerifyType7 :: Test
testVerifyType7 = myAssertEqual "verifyType a int" (True) (verifyType T_Template T_Int)

testVerifyType8 :: Test
testVerifyType8 = myAssertEqual "verifyType int NULL" (True) (verifyType T_Int T_NULL)

testVerifyType9 :: Test
testVerifyType9 = myAssertEqual "verifyType procedure <(int) => int>" (True) (verifyType T_Procedure (T_Function [T_Int] T_Int))

testVerifyType10 :: Test
testVerifyType10 = myAssertEqual "verifyType int|uint|char int" (True) (verifyType (T_Combination [T_Int, T_UInt, T_Char]) T_Int)

testVerifyType11 :: Test
testVerifyType11 = myAssertEqual "verifyType int|uint|char int|uint" (True) (verifyType (T_Combination [T_Int, T_UInt, T_Char]) (T_Combination [T_Int, T_UInt]))

testVerifyType12 :: Test
testVerifyType12 = myAssertEqual "verifyType int|uint|char int|uint|char" (True) (verifyType (T_Combination [T_Int, T_UInt, T_Char]) (T_Combination [T_Int, T_UInt, T_Char]))

--testVerifyType13 :: Test
--testVerifyType13 = myAssertEqual "verifyType int int|uint|char" (False) (verifyType T_Int (T_Combination [T_Int, T_UInt, T_Char]))

testVerifyType14 :: Test
testVerifyType14 = myAssertEqual "verifyType {int, int} {int, int}" (True) (verifyType (T_Tuple (T_Int, T_Int)) (T_Tuple (T_Int, T_Int)))

testVerifyType15 :: Test
testVerifyType15 = myAssertEqual "verifyType {int, int} {uint, int}" (False) (verifyType (T_Tuple (T_Int, T_Int)) (T_Tuple (T_UInt, T_Int)))

testVerifyType16 :: Test
testVerifyType16 = myAssertEqual "verifyType {int, int} {int, uint}" (False) (verifyType (T_Tuple (T_Int, T_Int)) (T_Tuple (T_Int, T_UInt)))

testVerifyType17 :: Test
testVerifyType17 = myAssertEqual "verifyType {int, int} {uint, uint}" (False) (verifyType (T_Tuple (T_Int, T_Int)) (T_Tuple (T_UInt, T_UInt)))

testVerifyType18 :: Test
testVerifyType18 = myAssertEqual "verifyType [int] []" (True) (verifyType (T_List T_Int) T_EmptyList)

testVerifyType19 :: Test
testVerifyType19 = myAssertEqual "verifyType [int] [int]" (True) (verifyType (T_List T_Int) (T_List T_Int))

testVerifyType20 :: Test
testVerifyType20 = myAssertEqual "verifyType [int] [uint]" (False) (verifyType (T_List T_Int) (T_List T_UInt))

testVerifyType21 :: Test
testVerifyType21 = myAssertEqual "verifyType int int" (True) (verifyType T_Int T_Int)

testVerifyType22 :: Test
testVerifyType22 = myAssertEqual "verifyType [char] string" (True) (verifyType (T_List T_Char) T_String)

testVerifyType23 :: Test
testVerifyType23 = myAssertEqual "verifyType string [char]" (True) (verifyType T_String (T_List T_Char))

testVerifyType24 :: Test
testVerifyType24 = myAssertEqual "verifyType string []" (True) (verifyType T_String T_EmptyList)

--testVerifyType25 :: Test
--testVerifyType25 = myAssertEqual "verifyType int int|uint|char" (False) (verifyType T_Int (T_Combination [T_Int, T_UInt, T_Char]))

testVerifyType26 :: Test
testVerifyType26 = myAssertEqual "verifyType [{int, int}] [{int, int}]" (True) (verifyType (T_List $ T_Tuple (T_Int, T_Int)) (T_List $ T_Tuple (T_Int, T_Int)))

testVerifyType27 :: Test
testVerifyType27 = myAssertEqual "verifyType [{int, int}] [{int, uint}]" (False) (verifyType (T_List $ T_Tuple (T_Int, T_Int)) (T_List $ T_Tuple (T_Int, T_UInt)))

testVerifyType28 :: Test
testVerifyType28 = myAssertEqual "verifyType [{uint, int}] [{int, int}]" (False) (verifyType (T_List $ T_Tuple (T_UInt, T_Int)) (T_List $ T_Tuple (T_Int, T_Int)))

testVerifyType29 :: Test
testVerifyType29 = myAssertEqual "verifyType [{int|uint, int}] [{int, int}]" (False) (verifyType (T_List $ T_Tuple (T_Combination [T_Int, T_UInt], T_Int)) (T_List $ T_Tuple (T_Int, T_Int)))

testVerifyType :: Test
testVerifyType = TestList [
    TestLabel "invalid parameter" testVerifyType1,
    TestLabel "invalid parameter" testVerifyType2,
    TestLabel "invalid parameter" testVerifyType3,
    TestLabel "invalid parameter" testVerifyType4,
    TestLabel "invalid argument" testVerifyType5,
    TestLabel "invalid argument" testVerifyType6,
    TestLabel "verifyType" testVerifyType7,
    TestLabel "verifyType" testVerifyType8,
    TestLabel "verifyType" testVerifyType9,
    TestLabel "verifyType" testVerifyType8,
    TestLabel "verifyType" testVerifyType9,
    TestLabel "verifyType" testVerifyType10,
    TestLabel "verifyType" testVerifyType11,
    TestLabel "verifyType" testVerifyType12,
--    TestLabel "verifyType" testVerifyType13,
    TestLabel "verifyType" testVerifyType14,
    TestLabel "verifyType" testVerifyType15,
    TestLabel "verifyType" testVerifyType16,
    TestLabel "verifyType" testVerifyType17,
    TestLabel "verifyType" testVerifyType18,
    TestLabel "verifyType" testVerifyType19,
    TestLabel "verifyType" testVerifyType20,
    TestLabel "verifyType" testVerifyType21,
    TestLabel "verifyType" testVerifyType22,
    TestLabel "verifyType" testVerifyType23,
    TestLabel "verifyType" testVerifyType24,
--    TestLabel "verifyType" testVerifyType25,
    TestLabel "verifyType" testVerifyType26,
    TestLabel "verifyType" testVerifyType27,
    TestLabel "verifyType" testVerifyType28,
    TestLabel "verifyType" testVerifyType29
    ]

-------------------------------------------------------------------------------

testType :: Test
testType = TestList [
    TestLabel "Type deriving Eq" testEqType,
    TestLabel "Type deriving Show" testShowType,
    TestLabel "verifyTypeList" testVerifyTypeList,
    TestLabel "combinateTypes" testCombinateTypes,
    TestLabel "verifyType" testVerifyType
    ]
