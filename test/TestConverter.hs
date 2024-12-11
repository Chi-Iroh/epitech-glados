module TestConverter (testConverter) where

import Test.HUnit
import UtilsForTests
import Utils
import SExpression
import AST
import Converter

testSexprSListHandling1 :: Test
testSexprSListHandling1 = myAssertEqual "sexprSListHandling '0'" (Value $ ASTNumber 0) (sexprSListHandling [SNumber 0])

testSexprSListHandling2 :: Test
testSexprSListHandling2 = myAssertEqual "sexprSListHandling 'x'" (Value $ ASTSymbol "x") (sexprSListHandling [SSymbol "x"])

testSexprSListHandling3 :: Test
testSexprSListHandling3 = myAssertEqual "sexprSListHandling '#t'" (Value $ ASTBoolean True) (sexprSListHandling [SSymbol "#t"])

testSexprSListHandling4 :: Test
testSexprSListHandling4 = myAssertEqual "sexprSListHandling '#f'" (Value $ ASTBoolean False) (sexprSListHandling [SSymbol "#f"])

testSexprSListHandling5 :: Test
testSexprSListHandling5 = myAssertEqual "sexprSListHandling '(define x 0)'" (Value $ ASTDefine "x" (ASTNumber 0)) (sexprSListHandling [SSymbol "define", SSymbol "x", SNumber 0])

testSexprSListHandling6 :: Test
testSexprSListHandling6 = myAssertEqual "sexprSListHandling '(+ 1 2)'" (Value $ ASTCall (FunctionCall "+") [ASTNumber 1, ASTNumber 2]) (sexprSListHandling [SSymbol "+", SNumber 1, SNumber 2])

testSexprSListHandling7 :: Test
testSexprSListHandling7 = myAssertEqual "sexprSListHandling '(lambda (a b) (+ a b))'" (Value $ ASTLambda [ASTSymbol "a", ASTSymbol "b"] (ASTCall (FunctionCall "+") [ASTSymbol "a", ASTSymbol "b"])) (sexprSListHandling [SSymbol "lambda", SList [SSymbol "a", SSymbol "b"], SList [SSymbol "+", SSymbol "a", SSymbol "b"]])

testSexprSListHandling8 :: Test
testSexprSListHandling8 = myAssertEqual "sexprSListHandling '((lambda (a b) (+ a b)) 1 2)'" (Value $ ASTCall (LambdaCall [ASTSymbol "a", ASTSymbol "b"] (ASTCall (FunctionCall "+") [ASTSymbol "a", ASTSymbol "b"])) [ASTNumber 1, ASTNumber 2]) (sexprSListHandling [SList [SSymbol "lambda", SList [SSymbol "a", SSymbol "b"], SList [SSymbol "+", SSymbol "a", SSymbol "b"]], SNumber 1, SNumber 2])

testSexprSListHandling9 :: Test
testSexprSListHandling9 = myAssertEqual "sexprSListHandling '(define (add a b) (+ a b))'" (Value $ ASTDefine "add" (ASTLambda [ASTSymbol "a", ASTSymbol "b"] (ASTCall (FunctionCall "+") [ASTSymbol "a", ASTSymbol "b"]))) (sexprSListHandling [SSymbol "define", SList [SSymbol "add", SSymbol "a", SSymbol "b"], SList [SSymbol "+", SSymbol "a", SSymbol "b"]])

testSexprSListHandling10 :: Test
testSexprSListHandling10 = myAssertEqual "sexprSListHandling '(define)'" (Value $ ASTSymbol "define") (sexprSListHandling [SSymbol "define"])

testSexprSListHandling11 :: Test
testSexprSListHandling11 = myAssertEqual "sexprSListHandling '(define x)'" (Error "GLaDOS: SyntaxError: Define expression must assign something to the defined symbol.\n") (sexprSListHandling [SSymbol "define", SSymbol "x"])

testSexprSListHandling12 :: Test
testSexprSListHandling12 = myAssertEqual "sexprSListHandling '(lambda () #t)'" (Value $ ASTLambda [] (ASTBoolean True)) (sexprSListHandling [SSymbol "lambda", SList [], SSymbol "#t"])

testSexprSListHandling13 :: Test
testSexprSListHandling13 = myAssertEqual "sexprSListHandling '(lambda)'" (Value $ ASTSymbol "lambda") (sexprSListHandling [SSymbol "lambda"])

testSexprSListHandling14 :: Test
testSexprSListHandling14 = myAssertEqual "sexprSListHandling '(lambda (a b))'" (Error "GLaDOS: SyntaxError: Not enough arguments to declare a lambda.\n") (sexprSListHandling [SSymbol "lambda", SList [SSymbol "a", SSymbol "b"]])

testSexprSListHandling15 :: Test
testSexprSListHandling15 = myAssertEqual "sexprSListHandling '(lambda #t)'" (Error "GLaDOS: SyntaxError: Not enough arguments to declare a lambda.\n") (sexprSListHandling [SSymbol "lambda", SSymbol "#t"])

testSexprSListHandling :: Test
testSexprSListHandling = TestList [
    TestLabel "sexprSListHandling" testSexprSListHandling1,
    TestLabel "sexprSListHandling" testSexprSListHandling2,
    TestLabel "sexprSListHandling" testSexprSListHandling3,
    TestLabel "sexprSListHandling" testSexprSListHandling4,
    TestLabel "sexprSListHandling" testSexprSListHandling5,
    TestLabel "sexprSListHandling" testSexprSListHandling6,
    TestLabel "sexprSListHandling" testSexprSListHandling7,
    TestLabel "sexprSListHandling" testSexprSListHandling8,
    TestLabel "sexprSListHandling" testSexprSListHandling9,
    TestLabel "sexprSListHandling" testSexprSListHandling10,
    TestLabel "sexprSListHandling" testSexprSListHandling11,
    TestLabel "sexprSListHandling" testSexprSListHandling12,
    TestLabel "sexprSListHandling" testSexprSListHandling13,
    TestLabel "sexprSListHandling" testSexprSListHandling14,
    TestLabel "sexprSListHandling" testSexprSListHandling15
    ]

-------------------------------------------------------------------------------

testConvert1 :: Test
testConvert1 = myAssertEqual "convert '0'" (Value [ASTNumber 0]) (convert $ Value [SNumber 0])

testConvert2 :: Test
testConvert2 = myAssertEqual "convert 'x'" (Value [ASTSymbol "x"]) (convert $ Value [SSymbol "x"])

testConvert3 :: Test
testConvert3 = myAssertEqual "convert '(define x 0)'" (Value [ASTDefine "x" (ASTNumber 0)]) (convert $ Value [ SList [(SSymbol "define"), (SSymbol "x"), (SNumber 0)]])

testConvert4 :: Test
testConvert4 = myAssertEqual "convert '()'" (Error "GLaDOS: ConverterError: Expected a list of at least one SExpr but got an empty list instead. [Converter.hs:12]\n") (convert $ Value [SList []])

testConvert5 :: Test
testConvert5 = myAssertEqual "convert 'Error test'" (Error "test") (convert $ Error "test")

testConvert6 :: Test
testConvert6 = myAssertEqual "convert ''" (Value []) (convert $ Value [])

testConvert7 :: Test
testConvert7 = myAssertEqual "convert '(+ (* (- 10 2) (mod 19 3)) (div 10 2))'" (Value [ASTCall (FunctionCall "+") [ASTCall (FunctionCall "*") [a', b'], c']]) (convert $ Value [SList [(SSymbol "+"), SList [(SSymbol "*"), a, b], c]])
    where
        a = SList [(SSymbol "-"), (SNumber 10), (SNumber 2)]
        b = SList [(SSymbol "mod"), (SNumber 19), (SNumber 3)]
        c = SList [(SSymbol "div"), (SNumber 10), (SNumber 2)]
        a' = ASTCall (FunctionCall "-") [ASTNumber 10, ASTNumber 2]
        b' = ASTCall (FunctionCall "mod") [ASTNumber 19, ASTNumber 3]
        c' = ASTCall (FunctionCall "div") [ASTNumber 10, ASTNumber 2]

testConvert8 :: Test
testConvert8 = myAssertEqual "convert '(if #f 4 #f)'" (Value [ASTCall (FunctionCall "if") [ASTBoolean False, ASTNumber 4, ASTBoolean False]]) (convert $ Value [SList [(SSymbol "if"), (SSymbol "#f"), (SNumber 4), (SSymbol "#f")]])

testConvert9 :: Test
testConvert9 = myAssertEqual "convert '((lambda (a b) (+ a b)) 1 2)'" (Value [ASTCall (LambdaCall a' b') [ASTNumber 1,ASTNumber 2]]) (convert $ Value [SList [SList [(SSymbol "lambda"), a, b], (SNumber 1), (SNumber 2)]])
    where
        a = SList [(SSymbol "a"), (SSymbol "b")]
        b = SList [(SSymbol "+"), (SSymbol "a"), (SSymbol "b")]
        a' = [ASTSymbol "a",ASTSymbol "b"]
        b' = ASTCall (FunctionCall "+") [ASTSymbol "a",ASTSymbol "b"]

testConvert10 :: Test
testConvert10 = myAssertEqual "convert '(define (< a b)\n    #t\n)'" (Value [ASTDefine "<" a']) (convert $ Value [SList [(SSymbol "define"), a, (SSymbol "#t")]])
    where
        a = SList [(SSymbol "<"), (SSymbol "a"), (SSymbol "b")]
        a' = ASTLambda [ASTSymbol "a",ASTSymbol "b"] (ASTBoolean True)

testConvert :: Test
testConvert = TestList [
    TestLabel "convert" testConvert1,
    TestLabel "convert" testConvert2,
    TestLabel "convert" testConvert3,
    TestLabel "convert" testConvert4,
    TestLabel "convert" testConvert5,
    TestLabel "convert" testConvert6,
    TestLabel "convert" testConvert7,
    TestLabel "convert" testConvert8,
    TestLabel "convert" testConvert9,
    TestLabel "convert" testConvert10
    ]

-------------------------------------------------------------------------------

testConverter :: Test
testConverter = TestList [
    TestLabel "sexprSListHandling" testSexprSListHandling,
    TestLabel "convert" testConvert
    ]
