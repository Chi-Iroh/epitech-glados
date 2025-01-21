module TestParser (testParser) where

import Test.HUnit
import UtilsForTests
import Utils
import SExpression
import Parser

testConvertToASExpr1 :: Test
testConvertToASExpr1 = myAssertEqual "convertToASExpr '0'" (Value $ [(ASExpr $ SNumber 0)]) (convertToASExpr "0")

testConvertToASExpr2 :: Test
testConvertToASExpr2 = myAssertEqual "convertToASExpr 'x'" (Value $ [(ASExpr $ SSymbol "x")]) (convertToASExpr "x")

testConvertToASExpr3 :: Test
testConvertToASExpr3 = myAssertEqual "convertToASExpr '(define x 0)'" (Value $ [SListBegin, (ASExpr $ SSymbol "define x 0"), SListEnd]) (convertToASExpr "(define x 0)")

testConvertToASExpr4 :: Test
testConvertToASExpr4 = myAssertEqual "convertToASExpr '()'" (Value $ [SListBegin, SListEnd]) (convertToASExpr "()")

testConvertToASExpr5 :: Test
testConvertToASExpr5 = myAssertEqual "convertToASExpr '('" (Value $ [SListBegin]) (convertToASExpr "(")

testConvertToASExpr6 :: Test
testConvertToASExpr6 = myAssertEqual "convertToASExpr ')'" (Value $ [SListEnd]) (convertToASExpr ")")

testConvertToASExpr7 :: Test
testConvertToASExpr7 = myAssertEqual "convertToASExpr ''" (Value $ []) (convertToASExpr "")

testConvertToASExpr8 :: Test
testConvertToASExpr8 = myAssertEqual "convertToASExpr '(+ (* (- 10 2) (mod 19 3)) (div 10 2))'" (Value $ [SListBegin, (ASExpr $ SSymbol "+ (* (- 10 2) (mod 19 3)) (div 10 2"), SListEnd, SListEnd]) (convertToASExpr "(+ (* (- 10 2) (mod 19 3)) (div 10 2))")

testConvertToASExpr9 :: Test
testConvertToASExpr9 = myAssertEqual "convertToASExpr '(if #f 4 #f)'" (Value $ [SListBegin, (ASExpr $ SSymbol "if #f 4 #f"), SListEnd]) (convertToASExpr "(if #f 4 #f)")

testConvertToASExpr10 :: Test
testConvertToASExpr10 = myAssertEqual "convertToASExpr '((lambda (a b) (+ a b)) 1 2)'" (Value $ [SListBegin, SListBegin, (ASExpr $ SSymbol "lambda (a b) (+ a b)) 1 2"), SListEnd]) (convertToASExpr "((lambda (a b) (+ a b)) 1 2)")

testConvertToASExpr11 :: Test
testConvertToASExpr11 = myAssertEqual "convertToASExpr '(define (< a b)\n    #t\n)'" (Value $ [SListBegin, (ASExpr $ SSymbol "define (< a b)\n    #t\n"), SListEnd]) (convertToASExpr "(define (< a b)\n    #t\n)")

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
testStringToASExpr1 = myAssertEqual "stringToASExpr '0'" (Value $ [(ASExpr $ SNumber 0)]) (stringToASExpr (words "0") [])

testStringToASExpr2 :: Test
testStringToASExpr2 = myAssertEqual "stringToASExpr 'x'" (Value $ [(ASExpr $ SSymbol "x")]) (stringToASExpr (words "x") [])

testStringToASExpr3 :: Test
testStringToASExpr3 = myAssertEqual "stringToASExpr '(define x 0)'" (Value $ [SListBegin, (ASExpr $ SSymbol "define"), (ASExpr $ SSymbol "x"), (ASExpr $ SNumber 0), SListEnd]) (stringToASExpr (words "(define x 0)") [])

testStringToASExpr4 :: Test
testStringToASExpr4 = myAssertEqual "stringToASExpr '()'" (Value $ [SListBegin, SListEnd]) (stringToASExpr (words "()") [])

testStringToASExpr5 :: Test
testStringToASExpr5 = myAssertEqual "stringToASExpr '('" (Value $ [SListBegin]) (stringToASExpr (words "(") [])

testStringToASExpr6 :: Test
testStringToASExpr6 = myAssertEqual "stringToASExpr ')'" (Value $ [SListEnd]) (stringToASExpr (words ")") [])

testStringToASExpr7 :: Test
testStringToASExpr7 = myAssertEqual "stringToASExpr ''" (Value $ []) (stringToASExpr (words "") [])

testStringToASExpr8 :: Test
testStringToASExpr8 = myAssertEqual "stringToASExpr '(+ (* (- 10 2) (mod 19 3)) (div 10 2))'" (Value $ [SListBegin, (ASExpr $ SSymbol "+"), SListBegin, (ASExpr $ SSymbol "*")] ++ a ++ b ++ [SListEnd] ++ c ++ [SListEnd]) (stringToASExpr (words "(+ (* (- 10 2) (mod 19 3)) (div 10 2))") [])
    where
        a = [SListBegin, (ASExpr $ SSymbol "-"), (ASExpr $ SNumber 10), (ASExpr $ SNumber 2), SListEnd]
        b = [SListBegin, (ASExpr $ SSymbol "mod"), (ASExpr $ SNumber 19), (ASExpr $ SNumber 3), SListEnd]
        c = [SListBegin, (ASExpr $ SSymbol "div"), (ASExpr $ SNumber 10), (ASExpr $ SNumber 2), SListEnd]

testStringToASExpr9 :: Test
testStringToASExpr9 = myAssertEqual "stringToASExpr '(if #f 4 #f)'" (Value $ [SListBegin, (ASExpr $ SSymbol "if"), (ASExpr $ SSymbol "#f"), (ASExpr $ SNumber 4), (ASExpr $ SSymbol "#f"), SListEnd]) (stringToASExpr (words "(if #f 4 #f)") [])

testStringToASExpr10 :: Test
testStringToASExpr10 = myAssertEqual "stringToASExpr '((lambda (a b) (+ a b)) 1 2)'" (Value $ [SListBegin, SListBegin, (ASExpr $ SSymbol "lambda")] ++ a ++ b ++ [SListEnd, (ASExpr $ SNumber 1), (ASExpr $ SNumber 2), SListEnd]) (stringToASExpr (words "((lambda (a b) (+ a b)) 1 2)") [])
    where
        a = [SListBegin, (ASExpr $ SSymbol "a"), (ASExpr $ SSymbol "b"), SListEnd]
        b = [SListBegin, (ASExpr $ SSymbol "+"), (ASExpr $ SSymbol "a"), (ASExpr $ SSymbol "b"), SListEnd]

testStringToASExpr11 :: Test
testStringToASExpr11 = myAssertEqual "stringToASExpr '(define (< a b)\n    #t\n)'" (Value $ [SListBegin, (ASExpr $ SSymbol "define")] ++ a ++ [(ASExpr $ SSymbol "#t"), SListEnd]) (stringToASExpr (words "(define (< a b)\n    #t\n)") [])
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

testFromSafeSExpr1 :: Test
testFromSafeSExpr1 = myAssertEqual "fromSafeSExpr $ Value [(SSymbol 'x'), (SNumber 0)]" (Value $ SList [(SSymbol "x"), (SNumber 0)]) (fromSafeSExpr $ Value [(SSymbol "x"), (SNumber 0)])

testFromSafeSExpr2 :: Test
testFromSafeSExpr2 = myAssertEqual "fromSafeSExpr $ Value [(SSymbol 'x')]" (Value $ SList [(SSymbol "x")]) (fromSafeSExpr $ Value [(SSymbol "x")])

testFromSafeSExpr3 :: Test
testFromSafeSExpr3 = myAssertEqual "fromSafeSExpr $ Value []" (Value $ SList []) (fromSafeSExpr $ Value [])

testFromSafeSExpr4 :: Test
testFromSafeSExpr4 = myAssertEqual "fromSafeSExpr $ Value []" (Error "pikachu I choose you") (fromSafeSExpr $ Error "pikachu I choose you")

testFromSafeSExpr :: Test
testFromSafeSExpr = TestList [
    TestLabel "fromSafeSExpr" testFromSafeSExpr1,
    TestLabel "fromSafeSExpr" testFromSafeSExpr2,
    TestLabel "fromSafeSExpr" testFromSafeSExpr3,
    TestLabel "fromSafeSExpr" testFromSafeSExpr4
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

testParse1a :: Test
testParse1a = myAssertEqual "parse '0'" (Value [SNumber 0]) (parse "0")

testParse1b :: Test
testParse1b = myAssertEqual "parse '0i'" (Value [SNumber 0]) (parse "0i")

testParse1c :: Test
testParse1c = myAssertEqual "parse '0u'" (Value [SUint 0]) (parse "0u")

testParse1d :: Test
testParse1d = myAssertEqual "parse '0c'" (Value [SChar '\0']) (parse "0c")

testParse1e :: Test
testParse1e = myAssertEqual "parse '0.0'" (Value [SFloat 0.0]) (parse "0.0")

testParse1f :: Test
testParse1f = myAssertEqual "parse '0.5'" (Value [SFloat 0.5]) (parse "0.5")

testParse1g :: Test
testParse1g = myAssertEqual "parse '\"x\"'" (Value [SString "x"]) (parse "\"x\"")

testParse1h :: Test
testParse1h = myAssertEqual "parse ''x''" (Value [SChar 'x']) (parse "'x'")

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

testParse12 :: Test
testParse12 = myAssertEqual "parse '{}'" (Error "GLaDOS: SyntaxError: Invalid tuple detected") (parse "{}")

testParse13 :: Test
testParse13 = myAssertEqual "parse '{'" (Error "GLaDOS: SyntaxError: unexpected EOF while parsing, '}' expected\n") (parse "{")

testParse14 :: Test
testParse14 = myAssertEqual "parse '}'" (Error "SyntaxError: Unexpecting closing curly bracket found") (parse "}")

testParse15 :: Test
testParse15 = myAssertEqual "parse '{0, 0}'" (Value [STuple (SNumber 0:SNumber 0:[])]) (parse "{0, 0}")

testParse16 :: Test
testParse16 = myAssertEqual "parse '{0 0}'" (Error "GLaDOS: SyntaxError: Invalid tuple detected") (parse "{0 0}")

testParse17 :: Test
testParse17 = myAssertEqual "parse '{0, 0'" (Error "GLaDOS: SyntaxError: unexpected EOF while parsing, '}' expected\n") (parse "{0, 0")

testParse18 :: Test
testParse18 = myAssertEqual "parse '0, 0}'" (Error "SyntaxError: Unexpecting closing curly bracket found") (parse "0, 0}")

testParse19 :: Test
testParse19 = myAssertEqual "parse '{{\"a\", 0}, 1}'" (Value [STuple (STuple (SString "a":SNumber 0:[]):SNumber 1:[])]) (parse "{{\"a\", 0}, 1}")

testParse20 :: Test
testParse20 = myAssertEqual "parse '{0, {\"b\", 1}}'" (Value [STuple (SNumber 0:STuple (SString "b":SNumber 1:[]):[])]) (parse "{0, {\"b\", 1}}")

testParse21 :: Test
testParse21 = myAssertEqual "parse '{{\"a\", 0}, {\"b\", 1}}'" (Value [STuple (STuple (SString "a":SNumber 0:[]):STuple (SString "b":SNumber 1:[]):[])]) (parse "{{\"a\", 0}, {\"b\", 1}}")

testParse22 :: Test
testParse22 = myAssertEqual "parse '{{\"a\", 0}, {\"b\", 1}'" (Error "GLaDOS: SyntaxError: unexpected EOF while parsing, '}' expected\n") (parse "{{\"a\", 0}, {\"b\", 1}")

testParse23 :: Test
testParse23 = myAssertEqual "parse '{\"a\", 0}, {\"b\", 1}}'" (Error "SyntaxError: Unexpecting closing curly bracket found") (parse "{\"a\", 0}, {\"b\", 1}}")

testParse24 :: Test
testParse24 = myAssertEqual "parse '{{\"a\", 0} {\"b\", 1}}'" (Error "GLaDOS: SyntaxError: Invalid tuple detected") (parse "{{\"a\", 0} {\"b\", 1}}")

testParse25 :: Test
testParse25 = myAssertEqual "parse '{{\"a\" 0}, {\"b\", 1}}'" (Error "GLaDOS: SyntaxError: Invalid tuple detected") (parse "{{\"a\" 0}, {\"b\", 1}}")

testParse26 :: Test
testParse26 = myAssertEqual "parse '[]'" (Value [SArray []]) (parse "[]")

testParse27 :: Test
testParse27 = myAssertEqual "parse '['" (Error "GLaDOS: SyntaxError: unexpected EOF while parsing, ']' expected\n") (parse "[")

testParse28 :: Test
testParse28 = myAssertEqual "parse ']'" (Error "SyntaxError: Unexpecting closing bracket found") (parse "]")

testParse29 :: Test
testParse29 = myAssertEqual "parse '[0]'" (Value [SArray [SNumber 0]]) (parse "[0]")

testParse30 :: Test
testParse30 = myAssertEqual "parse '[0, 1, 2, 3]'" (Value [SArray [SNumber 0, SNumber 1, SNumber 2, SNumber 3]]) (parse "[0, 1, 2, 3]")

testParse31 :: Test
testParse31 = myAssertEqual "parse '[0 1 2 3]'" (Error "GLaDOS: SyntaxError: Invalid list detected") (parse "[0 1 2 3]")

testParse32 :: Test
testParse32 = myAssertEqual "parse '[[], [0], [1, 2], 3, []]'" (Value [SArray [SArray [], SArray [SNumber 0], SArray [SNumber 1, SNumber 2], SNumber 3, SArray []]]) (parse "[[], [0], [1, 2], 3, []]")

testParse33 :: Test
testParse33 = myAssertEqual "parse '[[], [0], [1 2], 3, []]'" (Error "GLaDOS: SyntaxError: Missing delimiter in array") (parse "[[], [0], [1 2], 3, []]")

testParse34 :: Test
testParse34 = myAssertEqual "parse '[], [0], [1, 2], 3, []]'" (Error "SyntaxError: Unexpecting closing bracket found") (parse "[], [0], [1, 2], 3, []]")

testParse35 :: Test
testParse35 = myAssertEqual "parse '[[, [0], [1, 2], 3, []]'" (Error "GLaDOS: SyntaxError: unexpected EOF while parsing, ']' expected\n") (parse "[[, [0], [1, 2], 3, []]")

testParse36 :: Test
testParse36 = myAssertEqual "parse '[[], [0], [1, 2], 3, []'" (Error "GLaDOS: SyntaxError: unexpected EOF while parsing, ']' expected\n") (parse "[[], [0], [1, 2], 3, []]")

testParse37 :: Test
testParse37 = myAssertEqual "parse '<-(a::int b::int) => int->'" (Value [SFunctionType [SList [SSymbol "a::int", SSymbol "b::int"], SSymbol "=>", SSymbol "int"]]) (parse "<-(a::int b::int) => int->")

testParse38 :: Test
testParse38 = myAssertEqual "parse '\"test with spaces\"'" (Value [SString "test with spaces"]) (parse "\"test with spaces\"")

testParse39 :: Test
testParse39 = myAssertEqual "parse '([{({0, []}), 0}])'" (Value [SList [SArray [STuple [SList [STuple [SNumber 0, SArray []]], SNumber 0]]]]) (parse "([{({0, []}), 0}])")

testParse40 :: Test
testParse40 = myAssertEqual "parse '([{({0, [}]), 0}])'" (Error "GLaDOS: SyntaxError: Invalid list detected") (parse "([{({0, []}), 0}])")

testParse41 :: Test
testParse41 = myAssertEqual "parse '{([({0, []}), 0}])'" (Error "GLaDOS: SyntaxError: Invalid list detected") (parse "([{({0, []}), 0}])")

testParse42 :: Test
testParse42 = myAssertEqual "parse '([{({0, []}, )}])'" (Error "GLaDOS: SyntaxError: Invalid list detected") (parse "([{({0, []}), 0}])")

testParse43 :: Test
testParse43 = myAssertEqual "parse '{(0, [}, 0])'" (Error "GLaDOS: SyntaxError: Invalid list detected") (parse "([{({0, []}), 0}])")

testParse :: Test
testParse = TestList [
    TestLabel "parse" testParse1a,
    TestLabel "parse" testParse1b,
    TestLabel "parse" testParse1c,
    TestLabel "parse" testParse1d,
    TestLabel "parse" testParse1e,
    TestLabel "parse" testParse1f,
    TestLabel "parse" testParse1g,
    TestLabel "parse" testParse1h,
    TestLabel "parse" testParse2,
    TestLabel "parse" testParse3,
    TestLabel "parse" testParse4,
    TestLabel "parse" testParse5,
    TestLabel "parse" testParse6,
    TestLabel "parse" testParse7,
    TestLabel "parse" testParse8,
    TestLabel "parse" testParse9,
    TestLabel "parse" testParse10,
    TestLabel "parse" testParse11,
    TestLabel "parse" testParse12,
    TestLabel "parse" testParse13,
    TestLabel "parse" testParse14,
    TestLabel "parse" testParse15,
    TestLabel "parse" testParse16,
    TestLabel "parse" testParse17,
    TestLabel "parse" testParse18,
    TestLabel "parse" testParse19,
    TestLabel "parse" testParse20,
    TestLabel "parse" testParse21,
    TestLabel "parse" testParse22,
    TestLabel "parse" testParse23,
    TestLabel "parse" testParse24,
    TestLabel "parse" testParse25,
    TestLabel "parse" testParse26,
    TestLabel "parse" testParse27,
    TestLabel "parse" testParse28,
    TestLabel "parse" testParse29,
    TestLabel "parse" testParse30,
    TestLabel "parse" testParse31,
    TestLabel "parse" testParse32,
    TestLabel "parse" testParse33,
    TestLabel "parse" testParse34,
    TestLabel "parse" testParse35,
    TestLabel "parse" testParse36,
    TestLabel "parse" testParse37,
    TestLabel "parse" testParse38,
    TestLabel "parse" testParse39,
    TestLabel "parse" testParse40,
    TestLabel "parse" testParse41,
    TestLabel "parse" testParse42,
    TestLabel "parse" testParse43
    ]

-------------------------------------------------------------------------------

testParser :: Test
testParser = TestList [
    TestLabel "convertToASExpr" testConvertToASExpr,
    TestLabel "stringToASExpr" testStringToASExpr,
    TestLabel "testParseParanthese" testParseParanthese,
    TestLabel "fromSafeSExpr" testFromSafeSExpr,
    TestLabel "concatSafe" testConcatSafe,
    TestLabel "aSExprToSExpr" testASExprToSExpr,
    TestLabel "parse" testParse
    ]
