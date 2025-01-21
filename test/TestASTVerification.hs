module TestASTVerification (testASTVerification) where

import Test.HUnit
import UtilsForTests
import Type
import Utils
import AST
import ASTVerification

err :: Safe [AST]
err = Error "GLaDOS: TypeError: Incompatible types."

testASTVerification0 :: Test
testASTVerification0 = myAssertEqual "verifAST []" (a) (verifAST a)
    where
        a = Error "test"

testASTVerification1 :: Test
testASTVerification1 = myAssertEqual "verifAST []" (a) (verifAST a)
    where
        a = Value []

testASTVerification2 :: Test
testASTVerification2 = myAssertEqual "verifAST [0i]" (a) (verifAST a)
    where
        a = Value [ASTInt 0]

testASTVerification3 :: Test
testASTVerification3 = myAssertEqual "verifAST [0u]" (a) (verifAST a)
    where
        a = Value [ASTUInt 0]

testASTVerification4 :: Test
testASTVerification4 = myAssertEqual "verifAST [0c]" (a) (verifAST a)
    where
        a = Value [ASTChar '\0']

testASTVerification5 :: Test
testASTVerification5 = myAssertEqual "verifAST [0.0]" (a) (verifAST a)
    where
        a = Value [ASTFloat 0.0]

testASTVerification6 :: Test
testASTVerification6 = myAssertEqual "verifAST [#t]" (a) (verifAST a)
    where
        a = Value [ASTBool True]

testASTVerification7 :: Test
testASTVerification7 = myAssertEqual "verifAST [{NULL, NULL}]" (a) (verifAST a)
    where
        a = Value [ASTTuple (ASTNULL, ASTNULL)]

testASTVerification8 :: Test
testASTVerification8 = myAssertEqual "verifAST [[]]" (a) (verifAST a)
    where
        a = Value [ASTArray []]

testASTVerification9 :: Test
testASTVerification9 = myAssertEqual "verifAST [[NULL]]" (a) (verifAST a)
    where
        a = Value [ASTArray [ASTNULL]]

testASTVerification10 :: Test
testASTVerification10 = myAssertEqual "verifAST [[0, 1, 2]]" (a) (verifAST a)
    where
        a = Value [ASTArray [ASTInt 0, ASTInt 1, ASTInt 2]]

testASTVerification11 :: Test
testASTVerification11 = myAssertEqual "verifAST [[0, NULL, 2]]" (a) (verifAST a)
    where
        a = Value [ASTArray [ASTInt 0, ASTNULL, ASTInt 2]]

testASTVerification12 :: Test
testASTVerification12 = myAssertEqual "verifAST [[0, #t, 2]]" (err) (verifAST a)
    where
        a = Value [ASTArray [ASTInt 0, ASTBool True, ASTInt 2]]

testASTVerification13 :: Test
testASTVerification13 = myAssertEqual "verifAST [\"test\"]" (a) (verifAST a)
    where
        a = Value [ASTString "test"]

testASTVerification14 :: Test
testASTVerification14 = myAssertEqual "verifAST [var]" (a) (verifAST a)
    where
        a = Value [ASTProcedure "var"]

testASTVerification15 :: Test
testASTVerification15 = myAssertEqual "verifAST [NULL]" (a) (verifAST a)
    where
        a = Value [ASTNULL]

-------------------------------------------------------------------------------

testASTVerification16 :: Test
testASTVerification16 = myAssertEqual "verifAST [(define \"var\" int 0)]" (a) (verifAST a)
    where
        a = Value [ASTDefine "var" T_Int (ASTInt 0)]

testASTVerification17 :: Test
testASTVerification17 = myAssertEqual "verifAST [(define \"var\" int NULL)]" (a) (verifAST a)
    where
        a = Value [ASTDefine "var" T_Int ASTNULL]

testASTVerification18 :: Test
testASTVerification18 = myAssertEqual "verifAST [(define \"var\" float pi)]" (a) (verifAST a)
    where
        a = Value [ASTDefine "var" T_Float (ASTProcedure "pi")]

testASTVerification19 :: Test
testASTVerification19 = myAssertEqual "verifAST [(function f () 0 int)]" (a) (verifAST a)
    where
        a = Value [ASTFunction "f" [] (ASTInt 0) T_Int]

testASTVerification20 :: Test
testASTVerification20 = myAssertEqual "verifAST [(function f () NULL int)]" (a) (verifAST a)
    where
        a = Value [ASTFunction "f" [] ASTNULL T_Int]

testASTVerification21 :: Test
testASTVerification21 = myAssertEqual "verifAST [(function f (a::int) 0 int)]" (a) (verifAST a)
    where
        a = Value [ASTFunction "f" [(ASTProcedure "a", T_Int)] (ASTInt 0) T_Int]

testASTVerification22 :: Test
testASTVerification22 = myAssertEqual "verifAST [(function f (a::int) a int)]" (a) (verifAST a)
    where
        a = Value [ASTFunction "f" [(ASTProcedure "a", T_Int)] (ASTProcedure "a") T_Int]

testASTVerification23 :: Test
testASTVerification23 = myAssertEqual "verifAST [(lambda () 0 int)]" (a) (verifAST a)
    where
        a = Value [ASTLambda [] (ASTInt 0) T_Int]

testASTVerification24 :: Test
testASTVerification24 = myAssertEqual "verifAST [(lambda () NULL int)]" (a) (verifAST a)
    where
        a = Value [ASTLambda [] ASTNULL T_Int]

testASTVerification25 :: Test
testASTVerification25 = myAssertEqual "verifAST [(lambda (a::int) 0 int)]" (a) (verifAST a)
    where
        a = Value [ASTLambda [(ASTProcedure "a", T_Int)] (ASTInt 0) T_Int]

testASTVerification26 :: Test
testASTVerification26 = myAssertEqual "verifAST [(lambda (a::int) a int)]" (a) (verifAST a)
    where
        a = Value [ASTLambda [(ASTProcedure "a", T_Int)] (ASTProcedure "a") T_Int]

testASTVerification27 :: Test
testASTVerification27 = myAssertEqual "verifAST [(if (#t) (#t) (#f))]" (a) (verifAST a)
    where
        a = Value [ASTIf (ASTBool True) (ASTBool True) (ASTBool False)]

testASTVerification28 :: Test
testASTVerification28 = myAssertEqual "verifAST [(if (NULL) (#t) (#f))]" (a) (verifAST a)
    where
        a = Value [ASTIf ASTNULL (ASTBool True) (ASTBool False)]

testASTVerification29 :: Test
testASTVerification29 = myAssertEqual "verifAST [(+ 0 1)]" (a) (verifAST a)
    where
        a = Value [ASTCall (FunctionCall "+") [ASTInt 0, ASTInt 1]]

testASTVerification30 :: Test
testASTVerification30 = myAssertEqual "verifAST [(+ 0 NULL)]" (a) (verifAST a)
    where
        a = Value [ASTCall (FunctionCall "+") [ASTInt 0, ASTNULL]]

testASTVerification31 :: Test
testASTVerification31 = myAssertEqual "verifAST [(+ 0.0 pi)]" (a) (verifAST a)
    where
        a = Value [ASTCall (FunctionCall "+") [ASTFloat 0.0, ASTProcedure "pi"]]

testASTVerification32 :: Test
testASTVerification32 = myAssertEqual "verifAST [((lambda () 0 int))]" (a) (verifAST a)
    where
        a = Value [ASTCall (LambdaCall [] (ASTInt 0) T_Int) []]

testASTVerification33 :: Test
testASTVerification33 = myAssertEqual "verifAST [((lambda () NULL int))]" (a) (verifAST a)
    where
        a = Value [ASTCall (LambdaCall [] ASTNULL T_Int) []]

testASTVerification34 :: Test
testASTVerification34 = myAssertEqual "verifAST [((lambda (a::int) 0 int) 0)]" (a) (verifAST a)
    where
        a = Value [ASTCall (LambdaCall [(ASTProcedure "a", T_Int)] (ASTInt 0) T_Int) [ASTInt 0]]

testASTVerification35 :: Test
testASTVerification35 = myAssertEqual "verifAST [((lambda (a::int) a int) 0)]" (a) (verifAST a)
    where
        a = Value [ASTCall (LambdaCall [(ASTProcedure "a", T_Int)] (ASTProcedure "a") T_Int) [ASTInt 0]]

-------------------------------------------------------------------------------

testASTVerification36 :: Test
testASTVerification36 = myAssertEqual "verifAST [(define \"var\" int #t)]" (err) (verifAST a)
    where
        a = Value [ASTDefine "var" T_Int (ASTBool True)]

testASTVerification37 :: Test
testASTVerification37 = myAssertEqual "verifAST [(define \"var\" int pi)]" (err) (verifAST a)
    where
        a = Value [ASTDefine "var" T_Int (ASTProcedure "pi")]

testASTVerification38 :: Test
testASTVerification38 = myAssertEqual "verifAST [(define \"var\" integer pi)]" (err) (verifAST a)
    where
        a = Value [ASTDefine "var" typeInteger (ASTProcedure "pi")]

testASTVerification39 :: Test
testASTVerification39 = myAssertEqual "verifAST [(function f () #t int)]" (err) (verifAST a)
    where
        a = Value [ASTFunction "f" [] (ASTBool True) T_Int]

testASTVerification40 :: Test
testASTVerification40 = myAssertEqual "verifAST [(function f (a::float) a int)]" (err) (verifAST a)
    where
        a = Value [ASTFunction "f" [(ASTProcedure "a", T_Float)] (ASTProcedure "a") T_Int]

testASTVerification41 :: Test
testASTVerification41 = myAssertEqual "verifAST [(lambda () #t int)]" (err) (verifAST a)
    where
        a = Value [ASTLambda [] (ASTBool True) T_Int]

testASTVerification42 :: Test
testASTVerification42 = myAssertEqual "verifAST [(lambda (a::float) a int)]" (err) (verifAST a)
    where
        a = Value [ASTLambda [(ASTProcedure "a", T_Float)] (ASTProcedure "a") T_Int]

testASTVerification43 :: Test
testASTVerification43 = myAssertEqual "verifAST [(if ({#t, #f}) (#t) (#f))]" (err) (verifAST a)
    where
        a = Value [ASTIf (ASTTuple (ASTBool True, ASTBool False)) (ASTBool True) (ASTBool False)]

testASTVerification44 :: Test
testASTVerification44 = myAssertEqual "verifAST [(+ #t 1)]" (err) (verifAST a)
    where
        a = Value [ASTCall (FunctionCall "+") [ASTBool True, ASTInt 1]]

testASTVerification45 :: Test
testASTVerification45 = myAssertEqual "verifAST [(+ 0 #t)]" (err) (verifAST a)
    where
        a = Value [ASTCall (FunctionCall "+") [ASTInt 0, ASTBool True]]

testASTVerification46 :: Test
testASTVerification46 = myAssertEqual "verifAST [((lambda () 0 int) NULL)]" (err) (verifAST a)
    where
        a = Value [ASTCall (LambdaCall [] (ASTInt 0) T_Int) [ASTNULL]]

testASTVerification47 :: Test
testASTVerification47 = myAssertEqual "verifAST [((lambda () #t int))]" (err) (verifAST a)
    where
        a = Value [ASTCall (LambdaCall [] (ASTBool True) T_Int) []]

testASTVerification48 :: Test
testASTVerification48 = myAssertEqual "verifAST [((lambda (a::int) a int) #t)]" (err) (verifAST a)
    where
        a = Value [ASTCall (LambdaCall [(ASTProcedure "a", T_Int)] (ASTProcedure "a") T_Int) [ASTBool True]]

testASTVerification49 :: Test
testASTVerification49 = myAssertEqual "verifAST [((lambda (a::float) a int) 0)]" (err) (verifAST a)
    where
        a = Value [ASTCall (LambdaCall [(ASTProcedure "a", T_Float)] (ASTProcedure "a") T_Int) [ASTProcedure "pi"]]

-------------------------------------------------------------------------------

testASTVerification50 :: Test
testASTVerification50 = myAssertEqual "verifAST [(define \"var1\" int 1),(define \"var2\" int 2)]" (a) (verifAST a)
    where
        a = Value [(ASTDefine "var1" T_Int (ASTInt 1)), (ASTDefine "var2" T_Int (ASTInt 2))]

testASTVerification51 :: Test
testASTVerification51 = myAssertEqual "verifAST [(define \"var2\" int (+ 2 1))]" (a) (verifAST a)
    where
        a = Value [(ASTDefine "var2" T_Int (ASTCall (FunctionCall "+") [ASTInt 2, ASTInt 1]))]

testASTVerification52 :: Test
testASTVerification52 = myAssertEqual "verifAST [(define \"res\" integer (if (#t) (1i) (1u)))]" (a) (verifAST a)
    where
        a = Value [(ASTDefine "res" typeInteger (ASTIf (ASTBool True) (ASTInt 1) (ASTUInt 1)))]

testASTVerification53 :: Test
testASTVerification53 = myAssertEqual "verifAST [(define \"var1\" int 1),(define \"var2\" int (+ 2 var1))]" (a) (verifAST a)
    where
        a = Value [(ASTDefine "var1" T_Int (ASTInt 1)), (ASTDefine "var2" T_Int (ASTCall (FunctionCall "+") [ASTInt 2, ASTProcedure "var1"]))]

testASTVerification54 :: Test
testASTVerification54 = myAssertEqual "verifAST [(define \"var1\" int 1),(define \"var2\" int (+ 2 var1)),(define \"res\" integer (if (< var1 var2) (* var2 (+ var1 1)) (1000u)))]" (a) (verifAST a)
    where
        a = Value [(ASTDefine "var1" T_Int (ASTInt 1)), (ASTDefine "var2" T_Int (ASTCall (FunctionCall "+") [ASTInt 2, ASTProcedure "var1"])), (ASTDefine "res" typeInteger (ASTIf (ASTCall (FunctionCall "<") [ASTProcedure "var1", ASTProcedure "var2"]) (ASTCall (FunctionCall "*") [ASTProcedure "var2", ASTCall (FunctionCall "+") [ASTProcedure "var1", ASTInt 1]]) (ASTUInt 1000)))]

-------------------------------------------------------------------------------

testASTVerification55 :: Test
testASTVerification55 = myAssertEqual "verifAST [(define \"var1\" int #t),(define \"var2\" int 2)]" (err) (verifAST a)
    where
        a = Value [(ASTDefine "var1" T_Int (ASTBool True)), (ASTDefine "var2" T_Int (ASTInt 2))]

testASTVerification56 :: Test
testASTVerification56 = myAssertEqual "verifAST [(define \"var1\" int 1),(define \"var2\" int #t)]" (err) (verifAST a)
    where
        a = Value [(ASTDefine "var1" T_Int (ASTInt 1)), (ASTDefine "var2" T_Int (ASTBool True))]

testASTVerification57 :: Test
testASTVerification57 = myAssertEqual "verifAST [(define \"var1\" int 1),(define \"var2\" int var3)]" (err) (verifAST a)
    where
        a = Value [(ASTDefine "var1" T_Int (ASTInt 1)), (ASTDefine "var2" T_Int (ASTProcedure "var3"))]

testASTVerification58 :: Test
testASTVerification58 = myAssertEqual "verifAST [(define \"var2\" bool (+ 2 1))]" (err) (verifAST a)
    where
        a = Value [(ASTDefine "var2" T_Bool (ASTCall (FunctionCall "+") [ASTInt 2, ASTInt 1]))]

testASTVerification59 :: Test
testASTVerification59 = myAssertEqual "verifAST [(define \"var2\" int (+ #t 1))]" (err) (verifAST a)
    where
        a = Value [(ASTDefine "var2" T_Int (ASTCall (FunctionCall "+") [ASTBool True, ASTInt 1]))]

testASTVerification60 :: Test
testASTVerification60 = myAssertEqual "verifAST [(define \"var2\" int (thisfunctiondoesnotexist 2 1))]" (err) (verifAST a)
    where
        a = Value [(ASTDefine "var2" T_Int (ASTCall (FunctionCall "thisfunctiondoesnotexist") [ASTInt 2, ASTInt 1]))]

testASTVerification61 :: Test
testASTVerification61 = myAssertEqual "verifAST [(define \"res\" integer (if (\"test\") (1i) (1u)))]" (err) (verifAST a)
    where
        a = Value [(ASTDefine "res" typeInteger (ASTIf (ASTString "test") (ASTInt 1) (ASTUInt 1)))]

testASTVerification62 :: Test
testASTVerification62 = myAssertEqual "verifAST [(define \"res\" integer (if (#t) (1i) (#t)))]" (err) (verifAST a)
    where
        a = Value [(ASTDefine "res" typeInteger (ASTIf (ASTBool True) (ASTInt 1) (ASTBool True)))]

testASTVerification63 :: Test
testASTVerification63 = myAssertEqual "verifAST [(define \"var1\" float 1.0),(define \"var2\" int (+ 2 var1))]" (err) (verifAST a)
    where
        a = Value [(ASTDefine "var1" T_Float (ASTFloat 1.0)), (ASTDefine "var2" T_Int (ASTCall (FunctionCall "+") [ASTInt 2, ASTProcedure "var1"]))]

-------------------------------------------------------------------------------

testASTVerification :: Test
testASTVerification = TestList [
    TestLabel "verifAST:basic" testASTVerification0,
    TestLabel "verifAST:basic" testASTVerification1,
    TestLabel "verifAST:basic" testASTVerification2,
    TestLabel "verifAST:basic" testASTVerification3,
    TestLabel "verifAST:basic" testASTVerification4,
    TestLabel "verifAST:basic" testASTVerification5,
    TestLabel "verifAST:basic" testASTVerification6,
    TestLabel "verifAST:basic" testASTVerification7,
    TestLabel "verifAST:basic" testASTVerification8,
    TestLabel "verifAST:basic" testASTVerification9,
    TestLabel "verifAST:basic" testASTVerification10,
    TestLabel "verifAST:basic" testASTVerification11,
    TestLabel "verifAST:basic" testASTVerification12,
    TestLabel "verifAST:basic" testASTVerification13,
    TestLabel "verifAST:basic" testASTVerification14,
    TestLabel "verifAST:basic" testASTVerification15,
    TestLabel "verifAST:simple correct" testASTVerification16,
    TestLabel "verifAST:simple correct" testASTVerification17,
    TestLabel "verifAST:simple correct" testASTVerification18,
    TestLabel "verifAST:simple correct" testASTVerification19,
    TestLabel "verifAST:simple correct" testASTVerification20,
    TestLabel "verifAST:simple correct" testASTVerification21,
    TestLabel "verifAST:simple correct" testASTVerification22,
    TestLabel "verifAST:simple correct" testASTVerification23,
    TestLabel "verifAST:simple correct" testASTVerification24,
    TestLabel "verifAST:simple correct" testASTVerification25,
    TestLabel "verifAST:simple correct" testASTVerification26,
    TestLabel "verifAST:simple correct" testASTVerification27,
    TestLabel "verifAST:simple correct" testASTVerification28,
    TestLabel "verifAST:simple correct" testASTVerification29,
    TestLabel "verifAST:simple correct" testASTVerification30,
    TestLabel "verifAST:simple correct" testASTVerification31,
    TestLabel "verifAST:simple correct" testASTVerification32,
    TestLabel "verifAST:simple correct" testASTVerification33,
    TestLabel "verifAST:simple correct" testASTVerification34,
    TestLabel "verifAST:simple correct" testASTVerification35,
    TestLabel "verifAST:simple wrong" testASTVerification36,
    TestLabel "verifAST:simple wrong" testASTVerification37,
    TestLabel "verifAST:simple wrong" testASTVerification38,
    TestLabel "verifAST:simple wrong" testASTVerification39,
    TestLabel "verifAST:simple wrong" testASTVerification40,
    TestLabel "verifAST:simple wrong" testASTVerification41,
    TestLabel "verifAST:simple wrong" testASTVerification42,
    TestLabel "verifAST:simple wrong" testASTVerification43,
    TestLabel "verifAST:simple wrong" testASTVerification44,
    TestLabel "verifAST:simple wrong" testASTVerification45,
    TestLabel "verifAST:simple wrong" testASTVerification46,
    TestLabel "verifAST:simple wrong" testASTVerification47,
    TestLabel "verifAST:simple wrong" testASTVerification48,
    TestLabel "verifAST:simple wrong" testASTVerification49,
    TestLabel "verifAST:advanced correct" testASTVerification50,
    TestLabel "verifAST:advanced correct" testASTVerification51,
    TestLabel "verifAST:advanced correct" testASTVerification52,
    TestLabel "verifAST:advanced correct" testASTVerification53,
    TestLabel "verifAST:advanced correct" testASTVerification54,
    TestLabel "verifAST:advanced wrong" testASTVerification55,
    TestLabel "verifAST:advanced wrong" testASTVerification56,
    TestLabel "verifAST:advanced wrong" testASTVerification57,
    TestLabel "verifAST:advanced wrong" testASTVerification58,
    TestLabel "verifAST:advanced wrong" testASTVerification59,
    TestLabel "verifAST:advanced wrong" testASTVerification60,
    TestLabel "verifAST:advanced wrong" testASTVerification61,
    TestLabel "verifAST:advanced wrong" testASTVerification62,
    TestLabel "verifAST:advanced wrong" testASTVerification63
    ]
