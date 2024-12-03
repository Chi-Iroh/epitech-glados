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

testParseParanthese1 :: Test
testParseParanthese1 = TestCase (assertEqual "parseParanthese '0)'" (Value ([], [(ASExpr $ SNumber 0)])) (parseParanthese [(ASExpr $ SNumber 0), SListEnd] [] 0))

testParseParanthese2 :: Test
testParseParanthese2 = TestCase (assertEqual "parseParanthese 'x)'" (Value ([], [(ASExpr $ SSymbol "x")])) (parseParanthese [(ASExpr $ SSymbol "x"), SListEnd] [] 0))

testParseParanthese3 :: Test
testParseParanthese3 = TestCase (assertEqual "parseParanthese 'define x 0)'" (Value ([], [(ASExpr $ SSymbol "define"), (ASExpr $ SSymbol "x"), (ASExpr $ SNumber 0)])) (parseParanthese [(ASExpr $ SSymbol "define"), (ASExpr $ SSymbol "x"), (ASExpr $ SNumber 0), SListEnd] [] 0))

testParseParanthese4 :: Test
testParseParanthese4 = TestCase (assertEqual "parseParanthese ')'" (Value ([], [])) (parseParanthese [SListEnd] [] 0))

testParseParanthese5 :: Test
testParseParanthese5 = TestCase (assertEqual "parseParanthese '('" (Error "GLaDOS: SyntaxError: unexpected EOF while parsing, ')' expected") (parseParanthese [SListBegin] [] 0))

testParseParanthese6 :: Test
testParseParanthese6 = TestCase (assertEqual "parseParanthese ''" (Error "GLaDOS: SyntaxError: unexpected EOF while parsing, ')' expected") (parseParanthese [] [] 0))

testParseParanthese :: Test
testParseParanthese = TestList [
    TestLabel "parseParanthese" testParseParanthese1,
    TestLabel "parseParanthese" testParseParanthese2,
    TestLabel "parseParanthese" testParseParanthese3,
    TestLabel "parseParanthese" testParseParanthese4,
    TestLabel "parseParanthese" testParseParanthese5,
    TestLabel "parseParanthese" testParseParanthese6
    ]

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

testConcatSafe1 :: Test
testConcatSafe1 = TestCase (assertEqual "concatSafe 'define' 'x 0'" (Value [(SSymbol "define"), (SSymbol "x"), (SNumber 0)]) (concatSafe (Value $ SSymbol "define") (Value [(SSymbol "x"), (SNumber 0)])))

testConcatSafe2 :: Test
testConcatSafe2 = TestCase (assertEqual "concatSafe 'define' ''" (Value [(SSymbol "define")]) (concatSafe (Value $ SSymbol "define") (Value [])))

testConcatSafe3 :: Test
testConcatSafe3 = TestCase (assertEqual "concatSafe 'Error' ''" (Error "pikachu I choose you") (concatSafe (Error "pikachu I choose you") (Value [])))

testConcatSafe4 :: Test
testConcatSafe4 = TestCase (assertEqual "concatSafe 'define' 'Error'" (Error "pikachu I choose you") (concatSafe (Value $ SSymbol "define") (Error "pikachu I choose you")))

testConcatSafe5 :: Test
testConcatSafe5 = TestCase (assertEqual "concatSafe 'Error A' 'Error B'" (Error "2 Errors encountered at the same time: A ; B") (concatSafe (Error "A") (Error "B")))

testConcatSafe :: Test
testConcatSafe = TestList [
    TestLabel "concatSafe" testConcatSafe1,
    TestLabel "concatSafe" testConcatSafe2,
    TestLabel "concatSafe" testConcatSafe3,
    TestLabel "concatSafe" testConcatSafe4,
    TestLabel "concatSafe" testConcatSafe5
    ]

-------------------------------------------------------------------------------

--verifyParanthese cannot be tested outside aSExprToSExpr

-------------------------------------------------------------------------------

testASExprToSExpr1 :: Test
testASExprToSExpr1 = TestCase $ assertEqual "aSExprToSExpr '0'" (Value [SNumber 0]) (aSExprToSExpr [(ASExpr $ SNumber 0)] (Value []))

testASExprToSExpr2 :: Test
testASExprToSExpr2 = TestCase $ assertEqual "aSExprToSExpr 'x'" (Value [SSymbol "x"]) (aSExprToSExpr [(ASExpr $ SSymbol "x")] (Value []))

testASExprToSExpr3 :: Test
testASExprToSExpr3 = TestCase $ assertEqual "aSExprToSExpr '(define x 0)'" (Value [ SList [(SSymbol "define"), (SSymbol "x"), (SNumber 0)]]) (aSExprToSExpr [SListBegin, (ASExpr $ SSymbol "define"), (ASExpr $ SSymbol "x"), (ASExpr $ SNumber 0), SListEnd] (Value []))

testASExprToSExpr4 :: Test
testASExprToSExpr4 = TestCase $ assertEqual "aSExprToSExpr '()'" (Value [SList []]) (aSExprToSExpr [SListBegin, SListEnd] (Value []))

testASExprToSExpr5 :: Test
testASExprToSExpr5 = TestCase $ assertEqual "aSExprToSExpr '('" (Error "GLaDOS: SyntaxError: unexpected EOF while parsing, ')' expected") (aSExprToSExpr [SListBegin] (Value []))

testASExprToSExpr6 :: Test
testASExprToSExpr6 = TestCase $ assertEqual "aSExprToSExpr ')'" (Error "GLaDOS: SyntaxError: unexpected ')' while parsing") (aSExprToSExpr [SListEnd] (Value []))

testASExprToSExpr7 :: Test
testASExprToSExpr7 = TestCase $ assertEqual "aSExprToSExpr ''" (Value []) (aSExprToSExpr [] (Value []))

testASExprToSExpr :: Test
testASExprToSExpr = TestList [
    TestLabel "aSExprToSExpr" testASExprToSExpr1,
    TestLabel "aSExprToSExpr" testASExprToSExpr2,
    TestLabel "aSExprToSExpr" testASExprToSExpr3,
    TestLabel "aSExprToSExpr" testASExprToSExpr4,
    TestLabel "aSExprToSExpr" testASExprToSExpr5,
    TestLabel "aSExprToSExpr" testASExprToSExpr6,
    TestLabel "aSExprToSExpr" testASExprToSExpr7
    ]

-------------------------------------------------------------------------------

testParse1 :: Test
testParse1 = TestCase $ assertEqual "parse '0'" (Value [SNumber 0]) (parse "0")

testParse2 :: Test
testParse2 = TestCase $ assertEqual "parse 'x'" (Value [SSymbol "x"]) (parse "x")

testParse3 :: Test
testParse3 = TestCase $ assertEqual "parse '(define x 0)'" (Value [ SList [(SSymbol "define"), (SSymbol "x"), (SNumber 0)]]) (parse "(define x 0)")

testParse4 :: Test
testParse4 = TestCase $ assertEqual "parse '()'" (Value [SList []]) (parse "()")

testParse5 :: Test
testParse5 = TestCase $ assertEqual "parse '('" (Error "GLaDOS: SyntaxError: unexpected EOF while parsing, ')' expected") (parse "(")

testParse6 :: Test
testParse6 = TestCase $ assertEqual "parse ')'" (Error "GLaDOS: SyntaxError: unexpected ')' while parsing") (parse ")")

testParse7 :: Test
testParse7 = TestCase $ assertEqual "parse ''" (Value []) (parse "")

testParse :: Test
testParse = TestList [
    TestLabel "parse" testParse1,
    TestLabel "parse" testParse2,
    TestLabel "parse" testParse3,
    TestLabel "parse" testParse4,
    TestLabel "parse" testParse5,
    TestLabel "parse" testParse6,
    TestLabel "parse" testParse7
    ]

-------------------------------------------------------------------------------

testParser :: Test
testParser = TestList [
    TestLabel "convertToASExpr" testConvertToASExpr,
    TestLabel "stringToASExpr" testStringToASExpr,
    TestLabel "testParseParanthese" testParseParanthese,
    TestLabel "fromSafe" testFromSafe,
    TestLabel "concatSafe" testConcatSafe,
    TestLabel "aSExprToSExpr" testASExprToSExpr,
    TestLabel "parse" testParse
    ]
