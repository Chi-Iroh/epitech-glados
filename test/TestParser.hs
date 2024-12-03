module TestParser (testParser) where

import Test.HUnit
import Utils
import SExpression
import Parser

testConvertToASExpr1 :: Test
testConvertToASExpr1 = TestCase (assertEqual "convertToASExpr '0'" [(ASExpr $ SNumber 0)] (convertToASExpr "0"))

testConvertToASExpr2 :: Test
testConvertToASExpr2 = TestCase (assertEqual "convertToASExpr 'x'" [(ASExpr $ SSymbol "x")] (convertToASExpr "x"))

testConvertToASExpr3 :: Test
testConvertToASExpr3 = TestCase (assertEqual "convertToASExpr '(define x 0)'" [SListBegin, (ASExpr $ SSymbol "define x 0"), SListEnd] (convertToASExpr "(define x 0)"))

testConvertToASExpr4 :: Test
testConvertToASExpr4 = TestCase (assertEqual "convertToASExpr '()'" [SListBegin, SListEnd] (convertToASExpr "()"))

testConvertToASExpr5 :: Test
testConvertToASExpr5 = TestCase (assertEqual "convertToASExpr '('" [SListBegin] (convertToASExpr "("))

testConvertToASExpr6 :: Test
testConvertToASExpr6 = TestCase (assertEqual "convertToASExpr ')'" [SListEnd] (convertToASExpr ")"))

testConvertToASExpr7 :: Test
testConvertToASExpr7 = TestCase (assertEqual "convertToASExpr ''" [] (convertToASExpr ""))

testConvertToASExpr :: Test
testConvertToASExpr = TestList [
    TestLabel "convertToASExpr" testConvertToASExpr1,
    TestLabel "convertToASExpr" testConvertToASExpr2,
    TestLabel "convertToASExpr" testConvertToASExpr3,
    TestLabel "convertToASExpr" testConvertToASExpr4,
    TestLabel "convertToASExpr" testConvertToASExpr5,
    TestLabel "convertToASExpr" testConvertToASExpr6,
    TestLabel "convertToASExpr" testConvertToASExpr7
    ]

-------------------------------------------------------------------------------

testStringToASExpr1 :: Test
testStringToASExpr1 = TestCase (assertEqual "stringToASExpr '0'" [(ASExpr $ SNumber 0)] (stringToASExpr (words "0") []))

testStringToASExpr2 :: Test
testStringToASExpr2 = TestCase (assertEqual "stringToASExpr 'x'" [(ASExpr $ SSymbol "x")] (stringToASExpr (words "x") []))

testStringToASExpr3 :: Test
testStringToASExpr3 = TestCase (assertEqual "stringToASExpr '(define x 0)'" [SListBegin, (ASExpr $ SSymbol "define"), (ASExpr $ SSymbol "x"), (ASExpr $ SNumber 0), SListEnd] (stringToASExpr (words "(define x 0)") []))

testStringToASExpr4 :: Test
testStringToASExpr4 = TestCase (assertEqual "stringToASExpr '()'" [SListBegin, SListEnd] (stringToASExpr (words "()") []))

testStringToASExpr5 :: Test
testStringToASExpr5 = TestCase (assertEqual "stringToASExpr '('" [SListBegin] (stringToASExpr (words "(") []))

testStringToASExpr6 :: Test
testStringToASExpr6 = TestCase (assertEqual "stringToASExpr ')'" [SListEnd] (stringToASExpr (words ")") []))

testStringToASExpr7 :: Test
testStringToASExpr7 = TestCase (assertEqual "stringToASExpr ''" [] (stringToASExpr (words "") []))

testStringToASExpr :: Test
testStringToASExpr = TestList [
    TestLabel "stringToASExpr" testStringToASExpr1,
    TestLabel "stringToASExpr" testStringToASExpr2,
    TestLabel "stringToASExpr" testStringToASExpr3,
    TestLabel "stringToASExpr" testStringToASExpr4,
    TestLabel "stringToASExpr" testStringToASExpr5,
    TestLabel "stringToASExpr" testStringToASExpr6,
    TestLabel "stringToASExpr" testStringToASExpr7
    ]

-------------------------------------------------------------------------------

--parseParanthese

-------------------------------------------------------------------------------

testFromSafe1 :: Test
testFromSafe1 = TestCase (assertEqual "fromSafe $ Value [(SSymbol 'x'), (SNumber 0)]" (Value $ SList [(SSymbol "x"), (SNumber 0)]) (fromSafe $ Value [(SSymbol "x"), (SNumber 0)]))

testFromSafe2 :: Test
testFromSafe2 = TestCase (assertEqual "fromSafe $ Value [(SSymbol 'x')]" (Value $ SList [(SSymbol "x")]) (fromSafe $ Value [(SSymbol "x")]))

testFromSafe3 :: Test
testFromSafe3 = TestCase (assertEqual "fromSafe $ Value []" (Value $ SList []) (fromSafe $ Value []))

testFromSafe4 :: Test
testFromSafe4 = TestCase (assertEqual "fromSafe $ Value []" (Error "pikachu I choose you") (fromSafe $ Error "pikachu I choose you"))

testFromSafe :: Test
testFromSafe = TestList [
    TestLabel "fromSafe" testFromSafe1,
    TestLabel "fromSafe" testFromSafe2,
    TestLabel "fromSafe" testFromSafe3,
    TestLabel "fromSafe" testFromSafe4
    ]

-------------------------------------------------------------------------------

--concatSafe

-------------------------------------------------------------------------------

--verifyParanthese

-------------------------------------------------------------------------------

--aSExprToSExpr

-------------------------------------------------------------------------------

--parse

-------------------------------------------------------------------------------

testParser :: Test
testParser = TestList [
    TestLabel "convertToASExpr" testConvertToASExpr,
    TestLabel "stringToASExpr" testStringToASExpr,
    TestLabel "fromSafe" testFromSafe
    ]
