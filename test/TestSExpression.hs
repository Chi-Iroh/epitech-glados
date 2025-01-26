module TestSExpression (testSExpression) where

import Test.HUnit
import UtilsForTests
import Utils
import SExpression

-------------------------------------------------------------------------------

testEqSExpr1 :: Test
testEqSExpr1 = myAssertEqual "SNumber" (True) ((SNumber 1) == (SNumber 1))

testEqSExpr2 :: Test
testEqSExpr2 = myAssertEqual "SSymbol" (True) ((SSymbol "1") == (SSymbol "1"))

testEqSExpr3 :: Test
testEqSExpr3 = myAssertEqual "SList" (True) ((SList []) == (SList []))

testEqSExpr4 :: Test
testEqSExpr4 = myAssertEqual "STuple" (True) ((STuple []) == (STuple []))

testEqSExpr5 :: Test
testEqSExpr5 = myAssertEqual "SArray" (True) ((SArray []) == (SArray []))

testEqSExpr6 :: Test
testEqSExpr6 = myAssertEqual "SFunctionType" (True) ((SFunctionType []) == (SFunctionType []))

testEqSExpr7 :: Test
testEqSExpr7 = myAssertEqual "SString" (True) ((SString "[]") == (SString "[]"))

testEqSExpr8 :: Test
testEqSExpr8 = myAssertEqual "SFloat" (True) ((SFloat 1.5) == (SFloat 1.5))

testEqSExpr9 :: Test
testEqSExpr9 = myAssertEqual "SUint" (True) ((SUint 1) == (SUint 1))

testEqSExpr10 :: Test
testEqSExpr10 = myAssertEqual "SChar" (True) ((SChar 'c') == (SChar 'c'))

testEqSExpr11 :: Test
testEqSExpr11 = myAssertEqual "SNumber == SSymbol" (False) ((SNumber 1) == (SSymbol "1"))

testEqSExpr12 :: Test
testEqSExpr12 = myAssertEqual "SNumber /= SSymbol" (True) ((SNumber 1) /= (SSymbol "1"))

testEqSExpr :: Test
testEqSExpr = TestList [
    TestLabel "SExpr deriving Eq" testEqSExpr1,
    TestLabel "SExpr deriving Eq" testEqSExpr2,
    TestLabel "SExpr deriving Eq" testEqSExpr3,
    TestLabel "SExpr deriving Eq" testEqSExpr4,
    TestLabel "SExpr deriving Eq" testEqSExpr5,
    TestLabel "SExpr deriving Eq" testEqSExpr6,
    TestLabel "SExpr deriving Eq" testEqSExpr7,
    TestLabel "SExpr deriving Eq" testEqSExpr8,
    TestLabel "SExpr deriving Eq" testEqSExpr9,
    TestLabel "SExpr deriving Eq" testEqSExpr10,
    TestLabel "SExpr deriving Eq" testEqSExpr11,
    TestLabel "SExpr deriving Eq" testEqSExpr12
    ]

-------------------------------------------------------------------------------

testShowSNumber :: Test
testShowSNumber = myAssertEqual "show SNumber" "SNumber 42" (show (SNumber 42))

testShowSSymbol :: Test
testShowSSymbol = myAssertEqual "show SSymbol" "SSymbol \"example\"" (show (SSymbol "example"))

testShowSList :: Test
testShowSList = myAssertEqual "show SList" "SList [SSymbol \"a\",SNumber 1]" (show (SList [SSymbol "a", SNumber 1]))

testShowSTuple :: Test
testShowSTuple = myAssertEqual "show STuple" "STuple [SSymbol \"x\",SNumber 10]" (show (STuple [SSymbol "x", SNumber 10]))

testShowSArray :: Test
testShowSArray = myAssertEqual "show SArray" "SArray [SNumber 1,SNumber 2,SNumber 3]" (show (SArray [SNumber 1, SNumber 2, SNumber 3]))

testShowSFunctionType :: Test
testShowSFunctionType = myAssertEqual "show SFunctionType" "SFunctionType [SSymbol \"Int\",SSymbol \"String\"]" (show (SFunctionType [SSymbol "Int", SSymbol "String"]))

testShowSString :: Test
testShowSString = myAssertEqual "show SString" "SString \"Hello\"" (show (SString "Hello"))

testShowSFloat :: Test
testShowSFloat = myAssertEqual "show SFloat" "SFloat 3.14" (show (SFloat 3.14))

testShowSUint :: Test
testShowSUint = myAssertEqual "show SUint" "SUint 100" (show (SUint 100))

testShowSChar :: Test
testShowSChar = myAssertEqual "show SChar" "SChar 'a'" (show (SChar 'a'))

testShowPrecSNumber :: Test
testShowPrecSNumber = myAssertEqual "showsPrec SNumber" "SNumber 42" (showsPrec 10 (SNumber 42) $ "")

testShowListSExpr :: Test
testShowListSExpr = myAssertEqual "showList SExpr" "[SSymbol \"a\",SNumber 1,SChar 'c']" (showList [SSymbol "a", SNumber 1, SChar 'c'] "")

testShowSExpr :: Test
testShowSExpr = TestList [
    TestLabel "show SNumber" testShowSNumber,
    TestLabel "show SSymbol" testShowSSymbol,
    TestLabel "show SList" testShowSList,
    TestLabel "show STuple" testShowSTuple,
    TestLabel "show SArray" testShowSArray,
    TestLabel "show SFunctionType" testShowSFunctionType,
    TestLabel "show SString" testShowSString,
    TestLabel "show SFloat" testShowSFloat,
    TestLabel "show SUint" testShowSUint,
    TestLabel "show SChar" testShowSChar,
    TestLabel "showsPrec" testShowPrecSNumber,
    TestLabel "showList" testShowListSExpr
    ]

-------------------------------------------------------------------------------

testGetSymbol1 :: Test
testGetSymbol1 = myAssertEqual "getSymbol with SSymbol" (Value "example") (getSymbol (SSymbol "example"))

testGetSymbol2 :: Test
testGetSymbol2 = myAssertEqual "getSymbol with non-SSymbol" (Error "SExpr is not a SSymbol.") (getSymbol (SNumber 42))

testGetSymbol :: Test
testGetSymbol = TestList [
    TestLabel "getSymbol with SSymbol" testGetSymbol1,
    TestLabel "getSymbol with non-SSymbol" testGetSymbol2
    ]

-------------------------------------------------------------------------------

testGetInteger1 :: Test
testGetInteger1 = myAssertEqual "getInteger with SNumber" (Value 42) (getInteger (SNumber 42))

testGetInteger2 :: Test
testGetInteger2 = myAssertEqual "getInteger with non-SNumber" (Error "SExpr is not a SNumber.") (getInteger (SSymbol "example"))

testGetInteger :: Test
testGetInteger = TestList [
    TestLabel "getInteger with SNumber" testGetInteger1,
    TestLabel "getInteger with non-SNumber" testGetInteger2
    ]

-------------------------------------------------------------------------------

testGetList1 :: Test
testGetList1 = myAssertEqual "getList with SList" (Value [SSymbol "a", SNumber 1]) (getList (SList [SSymbol "a", SNumber 1]))

testGetList2 :: Test
testGetList2 = myAssertEqual "getList with non-SList" (Error "SExpr is not a SList.") (getList (SSymbol "not a list"))

testGetList :: Test
testGetList = TestList [
    TestLabel "getList with SList" testGetList1,
    TestLabel "getList with non-SList" testGetList2
    ]

-------------------------------------------------------------------------------

testFromSymbol1 :: Test
testFromSymbol1 = myAssertEqual "fromSymbol with SSymbol" "example" (fromSymbol (SSymbol "example"))

testFromSymbol2 :: Test
testFromSymbol2 = myAssertEqual "fromSymbol with non-SSymbol" "" (fromSymbol (SNumber 42))

testFromSymbol :: Test
testFromSymbol = TestList [
    TestLabel "fromSymbol with SSymbol" testFromSymbol1,
    TestLabel "fromSymbol with non-SSymbol" testFromSymbol2
    ]

-------------------------------------------------------------------------------

testSExpression :: Test
testSExpression = TestList [
    TestLabel "SExpr deriving Eq" testEqSExpr,
    TestLabel "SExpr deriving Show" testShowSExpr,
    TestLabel "getSymbol" testGetSymbol,
    TestLabel "getInteger" testGetInteger,
    TestLabel "getList" testGetList,
    TestLabel "fromSymbol" testFromSymbol
    ]
