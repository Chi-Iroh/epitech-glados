module TestParser (testParser) where

import Test.HUnit
import UtilsForTests
import Utils
import SExpression
import Parser

testConvertToASExpr1 :: Test
testConvertToASExpr1 = myAssertEqual "convertToASExpr '0'" [(ASExpr $ SNumber 0)] (convertToASExpr "0")

testConvertToASExpr2 :: Test
testConvertToASExpr2 = myAssertEqual "convertToASExpr 'x'" [(ASExpr $ SSymbol "x")] (convertToASExpr "x")

testConvertToASExpr3 :: Test
testConvertToASExpr3 = myAssertEqual "convertToASExpr '(define x 0)'" [SListBegin, (ASExpr $ SSymbol "define x 0"), SListEnd] (convertToASExpr "(define x 0)")

testConvertToASExpr4 :: Test
testConvertToASExpr4 = myAssertEqual "convertToASExpr '()'" [SListBegin, SListEnd] (convertToASExpr "()")

testConvertToASExpr5 :: Test
testConvertToASExpr5 = myAssertEqual "convertToASExpr '('" [SListBegin] (convertToASExpr "(")

testConvertToASExpr6 :: Test
testConvertToASExpr6 = myAssertEqual "convertToASExpr ')'" [SListEnd] (convertToASExpr ")")

testConvertToASExpr7 :: Test
testConvertToASExpr7 = myAssertEqual "convertToASExpr ''" [] (convertToASExpr "")

testConvertToASExpr8 :: Test
testConvertToASExpr8 = myAssertEqual "convertToASExpr '(+ (* (- 10 2) (mod 19 3)) (div 10 2))'" [SListBegin, (ASExpr $ SSymbol "+ (* (- 10 2) (mod 19 3)) (div 10 2"), SListEnd, SListEnd] (convertToASExpr "(+ (* (- 10 2) (mod 19 3)) (div 10 2))")

testConvertToASExpr9 :: Test
testConvertToASExpr9 = myAssertEqual "convertToASExpr '(if #f 4 #f)'" [SListBegin, (ASExpr $ SSymbol "if #f 4 #f"), SListEnd] (convertToASExpr "(if #f 4 #f)")

testConvertToASExpr10 :: Test
testConvertToASExpr10 = myAssertEqual "convertToASExpr '((lambda (a b) (+ a b)) 1 2)'" [SListBegin, SListBegin, (ASExpr $ SSymbol "lambda (a b) (+ a b)) 1 2"), SListEnd] (convertToASExpr "((lambda (a b) (+ a b)) 1 2)")

testConvertToASExpr11 :: Test
testConvertToASExpr11 = myAssertEqual "convertToASExpr '(define (< a b)\n    #t\n)'" [SListBegin, (ASExpr $ SSymbol "define (< a b)\n    #t\n"), SListEnd] (convertToASExpr "(define (< a b)\n    #t\n)")

testConvertToASExpr :: Test
testConvertToASExpr = TestList [
    TestLabel "convertToASExpr" testConvertToASExpr1,
    TestLabel "convertToASExpr" testConvertToASExpr2,
    TestLabel "convertToASExpr" testConvertToASExpr3,
    TestLabel "convertToASExpr" testConvertToASExpr4,
    TestLabel "convertToASExpr" testConvertToASExpr5,
    TestLabel "convertToASExpr" testConvertToASExpr6,
    TestLabel "convertToASExpr" testConvertToASExpr7,
    TestLabel "convertToASExpr" testConvertToASExpr8,
    TestLabel "convertToASExpr" testConvertToASExpr9,
    TestLabel "convertToASExpr" testConvertToASExpr10,
    TestLabel "convertToASExpr" testConvertToASExpr11
    ]

-------------------------------------------------------------------------------

testStringToASExpr1 :: Test
testStringToASExpr1 = myAssertEqual "stringToASExpr '0'" [(ASExpr $ SNumber 0)] (stringToASExpr (words "0") [])

testStringToASExpr2 :: Test
testStringToASExpr2 = myAssertEqual "stringToASExpr 'x'" [(ASExpr $ SSymbol "x")] (stringToASExpr (words "x") [])

testStringToASExpr3 :: Test
testStringToASExpr3 = myAssertEqual "stringToASExpr '(define x 0)'" [SListBegin, (ASExpr $ SSymbol "define"), (ASExpr $ SSymbol "x"), (ASExpr $ SNumber 0), SListEnd] (stringToASExpr (words "(define x 0)") [])

testStringToASExpr4 :: Test
testStringToASExpr4 = myAssertEqual "stringToASExpr '()'" [SListBegin, SListEnd] (stringToASExpr (words "()") [])

testStringToASExpr5 :: Test
testStringToASExpr5 = myAssertEqual "stringToASExpr '('" [SListBegin] (stringToASExpr (words "(") [])

testStringToASExpr6 :: Test
testStringToASExpr6 = myAssertEqual "stringToASExpr ')'" [SListEnd] (stringToASExpr (words ")") [])

testStringToASExpr7 :: Test
testStringToASExpr7 = myAssertEqual "stringToASExpr ''" [] (stringToASExpr (words "") [])

testStringToASExpr8 :: Test
testStringToASExpr8 = myAssertEqual "stringToASExpr '(+ (* (- 10 2) (mod 19 3)) (div 10 2))'" ([SListBegin, (ASExpr $ SSymbol "+"), SListBegin, (ASExpr $ SSymbol "*")] ++ a ++ b ++ [SListEnd] ++ c ++ [SListEnd]) (stringToASExpr (words "(+ (* (- 10 2) (mod 19 3)) (div 10 2))") [])
    where
        a = [SListBegin, (ASExpr $ SSymbol "-"), (ASExpr $ SNumber 10), (ASExpr $ SNumber 2), SListEnd]
        b = [SListBegin, (ASExpr $ SSymbol "mod"), (ASExpr $ SNumber 19), (ASExpr $ SNumber 3), SListEnd]
        c = [SListBegin, (ASExpr $ SSymbol "div"), (ASExpr $ SNumber 10), (ASExpr $ SNumber 2), SListEnd]

testStringToASExpr9 :: Test
testStringToASExpr9 = myAssertEqual "stringToASExpr '(if #f 4 #f)'" [SListBegin, (ASExpr $ SSymbol "if"), (ASExpr $ SSymbol "#f"), (ASExpr $ SNumber 4), (ASExpr $ SSymbol "#f"), SListEnd] (stringToASExpr (words "(if #f 4 #f)") [])

testStringToASExpr10 :: Test
testStringToASExpr10 = myAssertEqual "stringToASExpr '((lambda (a b) (+ a b)) 1 2)'" ([SListBegin, SListBegin, (ASExpr $ SSymbol "lambda")] ++ a ++ b ++ [SListEnd, (ASExpr $ SNumber 1), (ASExpr $ SNumber 2), SListEnd]) (stringToASExpr (words "((lambda (a b) (+ a b)) 1 2)") [])
    where
        a = [SListBegin, (ASExpr $ SSymbol "a"), (ASExpr $ SSymbol "b"), SListEnd]
        b = [SListBegin, (ASExpr $ SSymbol "+"), (ASExpr $ SSymbol "a"), (ASExpr $ SSymbol "b"), SListEnd]

testStringToASExpr11 :: Test
testStringToASExpr11 = myAssertEqual "stringToASExpr '(define (< a b)\n    #t\n)'" ([SListBegin, (ASExpr $ SSymbol "define")] ++ a ++ [(ASExpr $ SSymbol "#t"), SListEnd]) (stringToASExpr (words "(define (< a b)\n    #t\n)") [])
    where
        a = [SListBegin, (ASExpr $ SSymbol "<"), (ASExpr $ SSymbol "a"), (ASExpr $ SSymbol "b"), SListEnd]

testStringToASExpr :: Test
testStringToASExpr = TestList [
    TestLabel "stringToASExpr" testStringToASExpr1,
    TestLabel "stringToASExpr" testStringToASExpr2,
    TestLabel "stringToASExpr" testStringToASExpr3,
    TestLabel "stringToASExpr" testStringToASExpr4,
    TestLabel "stringToASExpr" testStringToASExpr5,
    TestLabel "stringToASExpr" testStringToASExpr6,
    TestLabel "stringToASExpr" testStringToASExpr7,
    TestLabel "stringToASExpr" testStringToASExpr8,
    TestLabel "stringToASExpr" testStringToASExpr9,
    TestLabel "stringToASExpr" testStringToASExpr10,
    TestLabel "stringToASExpr" testStringToASExpr11
    ]

-------------------------------------------------------------------------------

testParseParanthese1 :: Test
testParseParanthese1 = myAssertEqual "parseParanthese '0)'" (Value ([], [(ASExpr $ SNumber 0)])) (parseParanthese [(ASExpr $ SNumber 0), SListEnd] [] 0)

testParseParanthese2 :: Test
testParseParanthese2 = myAssertEqual "parseParanthese 'x)'" (Value ([], [(ASExpr $ SSymbol "x")])) (parseParanthese [(ASExpr $ SSymbol "x"), SListEnd] [] 0)

testParseParanthese3 :: Test
testParseParanthese3 = myAssertEqual "parseParanthese 'define x 0)'" (Value ([], [(ASExpr $ SSymbol "define"), (ASExpr $ SSymbol "x"), (ASExpr $ SNumber 0)])) (parseParanthese [(ASExpr $ SSymbol "define"), (ASExpr $ SSymbol "x"), (ASExpr $ SNumber 0), SListEnd] [] 0)

testParseParanthese4 :: Test
testParseParanthese4 = myAssertEqual "parseParanthese ')'" (Value ([], [])) (parseParanthese [SListEnd] [] 0)

testParseParanthese5 :: Test
testParseParanthese5 = myAssertEqual "parseParanthese '('" (Error "GLaDOS: SyntaxError: unexpected EOF while parsing, ')' expected\n") (parseParanthese [SListBegin] [] 0)

testParseParanthese6 :: Test
testParseParanthese6 = myAssertEqual "parseParanthese ''" (Error "GLaDOS: SyntaxError: unexpected EOF while parsing, ')' expected\n") (parseParanthese [] [] 0)

testParseParanthese7 :: Test
testParseParanthese7 = myAssertEqual "parseParanthese ') (define x 0)'" (Value ([SListBegin, (ASExpr $ SSymbol "define"), (ASExpr $ SSymbol "x"), (ASExpr $ SNumber 0), SListEnd], [])) (parseParanthese [SListEnd, SListBegin, (ASExpr $ SSymbol "define"), (ASExpr $ SSymbol "x"), (ASExpr $ SNumber 0), SListEnd] [] 0)

testParseParanthese8 :: Test
testParseParanthese8 = myAssertEqual "parseParanthese '())'" (Value ([], [SListBegin, SListEnd])) (parseParanthese [SListBegin, SListEnd, SListEnd] [] 0)

testParseParanthese9 :: Test
testParseParanthese9 = myAssertEqual "parseParanthese '()) ()'" (Value ([SListBegin, SListEnd], [SListBegin, SListEnd])) (parseParanthese [SListBegin, SListEnd, SListEnd, SListBegin, SListEnd] [] 0)

testParseParanthese :: Test
testParseParanthese = TestList [
    TestLabel "parseParanthese" testParseParanthese1,
    TestLabel "parseParanthese" testParseParanthese2,
    TestLabel "parseParanthese" testParseParanthese3,
    TestLabel "parseParanthese" testParseParanthese4,
    TestLabel "parseParanthese" testParseParanthese5,
    TestLabel "parseParanthese" testParseParanthese6,
    TestLabel "parseParanthese" testParseParanthese7,
    TestLabel "parseParanthese" testParseParanthese8,
    TestLabel "parseParanthese" testParseParanthese9
    ]

-------------------------------------------------------------------------------

testFromSafe1 :: Test
testFromSafe1 = myAssertEqual "fromSafe $ Value [(SSymbol 'x'), (SNumber 0)]" (Value $ SList [(SSymbol "x"), (SNumber 0)]) (fromSafe $ Value [(SSymbol "x"), (SNumber 0)])

testFromSafe2 :: Test
testFromSafe2 = myAssertEqual "fromSafe $ Value [(SSymbol 'x')]" (Value $ SList [(SSymbol "x")]) (fromSafe $ Value [(SSymbol "x")])

testFromSafe3 :: Test
testFromSafe3 = myAssertEqual "fromSafe $ Value []" (Value $ SList []) (fromSafe $ Value [])

testFromSafe4 :: Test
testFromSafe4 = myAssertEqual "fromSafe $ Value []" (Error "pikachu I choose you") (fromSafe $ Error "pikachu I choose you")

testFromSafe :: Test
testFromSafe = TestList [
    TestLabel "fromSafe" testFromSafe1,
    TestLabel "fromSafe" testFromSafe2,
    TestLabel "fromSafe" testFromSafe3,
    TestLabel "fromSafe" testFromSafe4
    ]

-------------------------------------------------------------------------------

testConcatSafe1 :: Test
testConcatSafe1 = myAssertEqual "concatSafe 'define' 'x 0'" (Value [(SSymbol "define"), (SSymbol "x"), (SNumber 0)]) (concatSafe (Value $ SSymbol "define") (Value [(SSymbol "x"), (SNumber 0)]))

testConcatSafe2 :: Test
testConcatSafe2 = myAssertEqual "concatSafe 'define' ''" (Value [(SSymbol "define")]) (concatSafe (Value $ SSymbol "define") (Value []))

testConcatSafe3 :: Test
testConcatSafe3 = myAssertEqual "concatSafe 'Error' ''" (Error "pikachu I choose you") (concatSafe (Error "pikachu I choose you") (Value []))

testConcatSafe4 :: Test
testConcatSafe4 = myAssertEqual "concatSafe 'define' 'Error'" (Error "pikachu I choose you") (concatSafe (Value $ SSymbol "define") (Error "pikachu I choose you"))

testConcatSafe5 :: Test
testConcatSafe5 = myAssertEqual "concatSafe 'Error A' 'Error B'" (Error "2 Errors encountered at the same time: A ; B") (concatSafe (Error "A") (Error "B"))

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
testASExprToSExpr1 = myAssertEqual "aSExprToSExpr '0'" (Value [SNumber 0]) (aSExprToSExpr [(ASExpr $ SNumber 0)] (Value []))

testASExprToSExpr2 :: Test
testASExprToSExpr2 = myAssertEqual "aSExprToSExpr 'x'" (Value [SSymbol "x"]) (aSExprToSExpr [(ASExpr $ SSymbol "x")] (Value []))

testASExprToSExpr3 :: Test
testASExprToSExpr3 = myAssertEqual "aSExprToSExpr '(define x 0)'" (Value [ SList [(SSymbol "define"), (SSymbol "x"), (SNumber 0)]]) (aSExprToSExpr [SListBegin, (ASExpr $ SSymbol "define"), (ASExpr $ SSymbol "x"), (ASExpr $ SNumber 0), SListEnd] (Value []))

testASExprToSExpr4 :: Test
testASExprToSExpr4 = myAssertEqual "aSExprToSExpr '()'" (Value [SList []]) (aSExprToSExpr [SListBegin, SListEnd] (Value []))

testASExprToSExpr5 :: Test
testASExprToSExpr5 = myAssertEqual "aSExprToSExpr '('" (Error "GLaDOS: SyntaxError: unexpected EOF while parsing, ')' expected\n") (aSExprToSExpr [SListBegin] (Value []))

testASExprToSExpr6 :: Test
testASExprToSExpr6 = myAssertEqual "aSExprToSExpr ')'" (Error "GLaDOS: SyntaxError: unexpected ')' while parsing\n") (aSExprToSExpr [SListEnd] (Value []))

testASExprToSExpr7 :: Test
testASExprToSExpr7 = myAssertEqual "aSExprToSExpr ''" (Value []) (aSExprToSExpr [] (Value []))

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
testParse1 = myAssertEqual "parse '0'" (Value [SNumber 0]) (parse "0")

testParse2 :: Test
testParse2 = myAssertEqual "parse 'x'" (Value [SSymbol "x"]) (parse "x")

testParse3 :: Test
testParse3 = myAssertEqual "parse '(define x 0)'" (Value [ SList [(SSymbol "define"), (SSymbol "x"), (SNumber 0)]]) (parse "(define x 0)")

testParse4 :: Test
testParse4 = myAssertEqual "parse '()'" (Value [SList []]) (parse "()")

testParse5 :: Test
testParse5 = myAssertEqual "parse '('" (Error "GLaDOS: SyntaxError: unexpected EOF while parsing, ')' expected\n") (parse "(")

testParse6 :: Test
testParse6 = myAssertEqual "parse ')'" (Error "SyntaxError: Unexpecting closing paranthese found") (parse ")")

testParse7 :: Test
testParse7 = myAssertEqual "parse ''" (Value []) (parse "")

testParse8 :: Test
testParse8 = myAssertEqual "parse '(+ (* (- 10 2) (mod 19 3)) (div 10 2))'" (Value [SList [(SSymbol "+"), SList [(SSymbol "*"), a, b], c]]) (parse "(+ (* (- 10 2) (mod 19 3)) (div 10 2))")
    where
        a = SList [(SSymbol "-"), (SNumber 10), (SNumber 2)]
        b = SList [(SSymbol "mod"), (SNumber 19), (SNumber 3)]
        c = SList [(SSymbol "div"), (SNumber 10), (SNumber 2)]

testParse9 :: Test
testParse9 = myAssertEqual "parse '(if #f 4 #f)'" (Value [SList [(SSymbol "if"), (SSymbol "#f"), (SNumber 4), (SSymbol "#f")]]) (parse "(if #f 4 #f)")

testParse10 :: Test
testParse10 = myAssertEqual "parse '((lambda (a b) (+ a b)) 1 2)'" (Value [SList [SList [(SSymbol "lambda"), a, b], (SNumber 1), (SNumber 2)]]) (parse "((lambda (a b) (+ a b)) 1 2)")
    where
        a = SList [(SSymbol "a"), (SSymbol "b")]
        b = SList [(SSymbol "+"), (SSymbol "a"), (SSymbol "b")]

testParse11 :: Test
testParse11 = myAssertEqual "parse '(define (< a b)\n    #t\n)'" (Value [SList [(SSymbol "define"), a, (SSymbol "#t")]]) (parse "(define (< a b)\n    #t\n)")
    where
        a = SList [(SSymbol "<"), (SSymbol "a"), (SSymbol "b")]

testParse :: Test
testParse = TestList [
    TestLabel "parse" testParse1,
    TestLabel "parse" testParse2,
    TestLabel "parse" testParse3,
    TestLabel "parse" testParse4,
    TestLabel "parse" testParse5,
    TestLabel "parse" testParse6,
    TestLabel "parse" testParse7,
    TestLabel "parse" testParse8,
    TestLabel "parse" testParse9,
    TestLabel "parse" testParse10,
    TestLabel "parse" testParse11
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
