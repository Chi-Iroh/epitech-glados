module TestAST (testAST) where

import Test.HUnit
import UtilsForTests
import Type
import AST

-------------------------------------------------------------------------------

testASTEquality1 :: Test
testASTEquality1 = myAssertEqual "ASTInt equality"
    True
    (ASTInt 5 == ASTInt 5)

testASTEquality2 :: Test
testASTEquality2 = myAssertEqual "ASTInt inequality"
    False
    (ASTInt 5 == ASTInt 6)

testASTEquality3 :: Test
testASTEquality3 = myAssertEqual "ASTUInt equality"
    True
    (ASTUInt 5 == ASTUInt 5)

testASTEquality4 :: Test
testASTEquality4 = myAssertEqual "ASTUInt inequality"
    False
    (ASTUInt 5 == ASTUInt 6)

testASTEquality5 :: Test
testASTEquality5 = myAssertEqual "ASTChar equality"
    True
    (ASTChar 'a' == ASTChar 'a')

testASTEquality6 :: Test
testASTEquality6 = myAssertEqual "ASTChar inequality"
    False
    (ASTChar 'a' == ASTChar 'b')

testASTEquality7 :: Test
testASTEquality7 = myAssertEqual "ASTFloat equality"
    True
    (ASTFloat 5.5 == ASTFloat 5.5)

testASTEquality8 :: Test
testASTEquality8 = myAssertEqual "ASTFloat inequality"
    False
    (ASTFloat 5.5 == ASTFloat 6.5)

testASTEquality9 :: Test
testASTEquality9 = myAssertEqual "ASTBool equality"
    True
    (ASTBool True == ASTBool True)

testASTEquality10 :: Test
testASTEquality10 = myAssertEqual "ASTBool inequality"
    False
    (ASTBool True == ASTBool False)

testASTEquality11 :: Test
testASTEquality11 = myAssertEqual "ASTTuple equality"
    True
    (ASTTuple (ASTInt 5, ASTInt 10) == ASTTuple (ASTInt 5, ASTInt 10))

testASTEquality12 :: Test
testASTEquality12 = myAssertEqual "ASTTuple inequality"
    False
    (ASTTuple (ASTInt 5, ASTInt 10) == ASTTuple (ASTInt 6, ASTInt 10))

testASTEquality13 :: Test
testASTEquality13 = myAssertEqual "ASTArray equality"
    True
    (ASTArray [ASTInt 1, ASTInt 2, ASTInt 3] == ASTArray [ASTInt 1, ASTInt 2, ASTInt 3])

testASTEquality14 :: Test
testASTEquality14 = myAssertEqual "ASTArray inequality"
    False
    (ASTArray [ASTInt 1, ASTInt 2, ASTInt 3] == ASTArray [ASTInt 1, ASTInt 2, ASTInt 4])

testASTEquality15 :: Test
testASTEquality15 = myAssertEqual "ASTString equality"
    True
    (ASTString "hello" == ASTString "hello")

testASTEquality16 :: Test
testASTEquality16 = myAssertEqual "ASTString inequality"
    False
    (ASTString "hello" == ASTString "world")

testASTEquality17 :: Test
testASTEquality17 = myAssertEqual "ASTProcedure equality"
    True
    (ASTProcedure "proc1" == ASTProcedure "proc1")

testASTEquality18 :: Test
testASTEquality18 = myAssertEqual "ASTProcedure inequality"
    False
    (ASTProcedure "proc1" == ASTProcedure "proc2")

testASTEquality19 :: Test
testASTEquality19 = myAssertEqual "ASTDefine equality"
    True
    (ASTDefine "func" (T_Function [typeNumber] typeNumber) (ASTInt 5) ==
     ASTDefine "func" (T_Function [typeNumber] typeNumber) (ASTInt 5))

testASTEquality20 :: Test
testASTEquality20 = myAssertEqual "ASTDefine inequality"
    False
    (ASTDefine "func" (T_Function [typeNumber] typeNumber) (ASTInt 5) ==
     ASTDefine "func" (T_Function [typeNumber] typeNumber) (ASTInt 6))

testASTEquality21 :: Test
testASTEquality21 = myAssertEqual "ASTFunction equality"
    True
    (ASTFunction "func" [(ASTProcedure "x", T_Int)] (ASTInt 5) typeNumber ==
     ASTFunction "func" [(ASTProcedure "x", T_Int)] (ASTInt 5) typeNumber)

testASTEquality22 :: Test
testASTEquality22 = myAssertEqual "ASTFunction inequality"
    False
    (ASTFunction "func" [(ASTProcedure "x", T_Int)] (ASTInt 5) typeNumber ==
     ASTFunction "func" [(ASTProcedure "x", T_Int)] (ASTInt 6) typeNumber)

testASTEquality23 :: Test
testASTEquality23 = myAssertEqual "ASTLambda equality"
    True
    (ASTLambda [(ASTProcedure "x", T_Int)] (ASTInt 5) typeNumber ==
     ASTLambda [(ASTProcedure "x", T_Int)] (ASTInt 5) typeNumber)

testASTEquality24 :: Test
testASTEquality24 = myAssertEqual "ASTLambda inequality"
    False
    (ASTLambda [(ASTProcedure "x", T_Int)] (ASTInt 5) typeNumber ==
     ASTLambda [(ASTProcedure "x", T_Int)] (ASTInt 6) typeNumber)

testASTEquality25 :: Test
testASTEquality25 = myAssertEqual "ASTCall equality"
    True
    (ASTCall (FunctionCall "foo") [ASTInt 5, ASTInt 10] == ASTCall (FunctionCall "foo") [ASTInt 5, ASTInt 10])

testASTEquality26 :: Test
testASTEquality26 = myAssertEqual "ASTCall inequality"
    False
    (ASTCall (FunctionCall "foo") [ASTInt 5, ASTInt 10] == ASTCall (FunctionCall "foo") [ASTInt 6, ASTInt 10])

testASTEquality27 :: Test
testASTEquality27 = myAssertEqual "ASTIf equality"
    True
    (ASTIf (ASTBool True) (ASTInt 5) (ASTInt 10) == ASTIf (ASTBool True) (ASTInt 5) (ASTInt 10))

testASTEquality28 :: Test
testASTEquality28 = myAssertEqual "ASTIf inequality"
    False
    (ASTIf (ASTBool True) (ASTInt 5) (ASTInt 10) == ASTIf (ASTBool False) (ASTInt 5) (ASTInt 10))

testASTEquality29 :: Test
testASTEquality29 = myAssertEqual "ASTNULL equality"
    True
    (ASTNULL == ASTNULL)

testASTEquality30 :: Test
testASTEquality30 = myAssertEqual "ASTNULL inequality"
    False
    (ASTNULL == ASTInt 5)

testASTEquality31 :: Test
testASTEquality31 = myAssertEqual "ASTNULL inequality /="
    True
    (ASTNULL /= ASTInt 5)

testASTEquality :: Test
testASTEquality = TestList [
    TestLabel "ASTInt equality" testASTEquality1,
    TestLabel "ASTInt inequality" testASTEquality2,
    TestLabel "ASTUInt equality" testASTEquality3,
    TestLabel "ASTUInt inequality" testASTEquality4,
    TestLabel "ASTChar equality" testASTEquality5,
    TestLabel "ASTChar inequality" testASTEquality6,
    TestLabel "ASTFloat equality" testASTEquality7,
    TestLabel "ASTFloat inequality" testASTEquality8,
    TestLabel "ASTBool equality" testASTEquality9,
    TestLabel "ASTBool inequality" testASTEquality10,
    TestLabel "ASTTuple equality" testASTEquality11,
    TestLabel "ASTTuple inequality" testASTEquality12,
    TestLabel "ASTArray equality" testASTEquality13,
    TestLabel "ASTArray inequality" testASTEquality14,
    TestLabel "ASTString equality" testASTEquality15,
    TestLabel "ASTString inequality" testASTEquality16,
    TestLabel "ASTProcedure equality" testASTEquality17,
    TestLabel "ASTProcedure inequality" testASTEquality18,
    TestLabel "ASTDefine equality" testASTEquality19,
    TestLabel "ASTDefine inequality" testASTEquality20,
    TestLabel "ASTFunction equality" testASTEquality21,
    TestLabel "ASTFunction inequality" testASTEquality22,
    TestLabel "ASTLambda equality" testASTEquality23,
    TestLabel "ASTLambda inequality" testASTEquality24,
    TestLabel "ASTCall equality" testASTEquality25,
    TestLabel "ASTCall inequality" testASTEquality26,
    TestLabel "ASTIf equality" testASTEquality27,
    TestLabel "ASTIf inequality" testASTEquality28,
    TestLabel "ASTNULL equality" testASTEquality29,
    TestLabel "ASTNULL inequality" testASTEquality30,
    TestLabel "ASTNULL inequality /=" testASTEquality31
    ]

-------------------------------------------------------------------------------

testShowASTInt :: Test
testShowASTInt = myAssertEqual "Show ASTInt"
    "ASTInt 5"
    (show (ASTInt 5))

testShowASTUInt :: Test
testShowASTUInt = myAssertEqual "Show ASTUInt"
    "ASTUInt 5"
    (show (ASTUInt 5))

testShowASTChar :: Test
testShowASTChar = myAssertEqual "Show ASTChar"
    "ASTChar 'a'"
    (show (ASTChar 'a'))

testShowASTFloat :: Test
testShowASTFloat = myAssertEqual "Show ASTFloat"
    "ASTFloat 5.5"
    (show (ASTFloat 5.5))

testShowASTBool :: Test
testShowASTBool = myAssertEqual "Show ASTBool"
    "ASTBool True"
    (show (ASTBool True))

testShowASTTuple :: Test
testShowASTTuple = myAssertEqual "Show ASTTuple"
    "ASTTuple (ASTInt 5,ASTInt 10)"
    (show (ASTTuple (ASTInt 5, ASTInt 10)))

testShowASTArray :: Test
testShowASTArray = myAssertEqual "Show ASTArray"
    "ASTArray [ASTInt 1,ASTInt 2,ASTInt 3]"
    (show (ASTArray [ASTInt 1, ASTInt 2, ASTInt 3]))

testShowASTString :: Test
testShowASTString = myAssertEqual "Show ASTString"
    "ASTString \"hello\""
    (show (ASTString "hello"))

testShowASTProcedure :: Test
testShowASTProcedure = myAssertEqual "Show ASTProcedure"
    "ASTProcedure \"func1\""
    (show (ASTProcedure "func1"))

testShowASTDefine :: Test
testShowASTDefine = myAssertEqual "Show ASTDefine"
    "ASTDefine \"func\" <(int|uint|char|float) => int|uint|char|float> (ASTInt 5)"
    (show (ASTDefine "func" (T_Function [typeNumber] typeNumber) (ASTInt 5)))

testShowASTFunction :: Test
testShowASTFunction = myAssertEqual "Show ASTFunction"
    "ASTFunction \"func\" [(ASTProcedure \"x\",int)] (ASTInt 5) int|uint|char|float"
    (show (ASTFunction "func" [(ASTProcedure "x", T_Int)] (ASTInt 5) typeNumber))

testShowASTLambda :: Test
testShowASTLambda = myAssertEqual "Show ASTLambda"
    "ASTLambda [(ASTProcedure \"x\",int)] (ASTInt 5) int|uint|char|float"
    (show (ASTLambda [(ASTProcedure "x", T_Int)] (ASTInt 5) typeNumber))

testShowASTCall :: Test
testShowASTCall = myAssertEqual "Show ASTCall"
    "ASTCall (FunctionCall \"foo\") [ASTInt 5,ASTInt 10]"
    (show (ASTCall (FunctionCall "foo") [ASTInt 5, ASTInt 10]))

testShowASTIf :: Test
testShowASTIf = myAssertEqual "Show ASTIf"
    "ASTIf (ASTBool True) (ASTInt 5) (ASTInt 10)"
    (show (ASTIf (ASTBool True) (ASTInt 5) (ASTInt 10)))

testShowASTNULL :: Test
testShowASTNULL = myAssertEqual "Show ASTNULL"
    "ASTNULL"
    (show ASTNULL)

testShowsPrecASTTuple :: Test
testShowsPrecASTTuple = myAssertEqual "ShowsPrec ASTTuple"
    "ASTTuple (ASTInt 5,ASTInt 10)"
    (showsPrec 0 (ASTTuple (ASTInt 5, ASTInt 10)) "")

testShowListASTArray :: Test
testShowListASTArray = myAssertEqual "ShowList ASTArray"
    "[ASTInt 1,ASTInt 2,ASTInt 3]"
    (showList [ASTInt 1, ASTInt 2, ASTInt 3] "")

testShowAST :: Test
testShowAST = TestList [
    TestLabel "Show ASTInt" testShowASTInt,
    TestLabel "Show ASTUInt" testShowASTUInt,
    TestLabel "Show ASTChar" testShowASTChar,
    TestLabel "Show ASTFloat" testShowASTFloat,
    TestLabel "Show ASTBool" testShowASTBool,
    TestLabel "Show ASTTuple" testShowASTTuple,
    TestLabel "Show ASTArray" testShowASTArray,
    TestLabel "Show ASTString" testShowASTString,
    TestLabel "Show ASTProcedure" testShowASTProcedure,
    TestLabel "Show ASTDefine" testShowASTDefine,
    TestLabel "Show ASTFunction" testShowASTFunction,
    TestLabel "Show ASTLambda" testShowASTLambda,
    TestLabel "Show ASTCall" testShowASTCall,
    TestLabel "Show ASTIf" testShowASTIf,
    TestLabel "Show ASTNULL" testShowASTNULL,
    TestLabel "showsPrec" testShowsPrecASTTuple,
    TestLabel "showList" testShowListASTArray
    ]

-------------------------------------------------------------------------------

testGetTypeFunctionCall1 :: Test
testGetTypeFunctionCall1 = myAssertEqual "getTypeFunctionCall with +"
    (T_Function [typeNumber, typeNumber] typeNumber)
    (getTypeFunctionCall "+" [])

testGetTypeFunctionCall2 :: Test
testGetTypeFunctionCall2 = myAssertEqual "getTypeFunctionCall with add"
    (T_Function [typeNumber, typeNumber] typeNumber)
    (getTypeFunctionCall "add" [])

testGetTypeFunctionCall3 :: Test
testGetTypeFunctionCall3 = myAssertEqual "getTypeFunctionCall with -"
    (T_Function [typeNumber, typeNumber] typeNumber)
    (getTypeFunctionCall "-" [])

testGetTypeFunctionCall4 :: Test
testGetTypeFunctionCall4 = myAssertEqual "getTypeFunctionCall with sub"
    (T_Function [typeNumber, typeNumber] typeNumber)
    (getTypeFunctionCall "sub" [])

testGetTypeFunctionCall5 :: Test
testGetTypeFunctionCall5 = myAssertEqual "getTypeFunctionCall with *"
    (T_Function [typeNumber, typeNumber] typeNumber)
    (getTypeFunctionCall "*" [])

testGetTypeFunctionCall6 :: Test
testGetTypeFunctionCall6 = myAssertEqual "getTypeFunctionCall with mul"
    (T_Function [typeNumber, typeNumber] typeNumber)
    (getTypeFunctionCall "mul" [])

testGetTypeFunctionCall7 :: Test
testGetTypeFunctionCall7 = myAssertEqual "getTypeFunctionCall with /"
    (T_Function [typeNumber, typeNumber] typeNumber)
    (getTypeFunctionCall "/" [])

testGetTypeFunctionCall8 :: Test
testGetTypeFunctionCall8 = myAssertEqual "getTypeFunctionCall with div"
    (T_Function [typeNumber, typeNumber] typeNumber)
    (getTypeFunctionCall "div" [])

testGetTypeFunctionCall9 :: Test
testGetTypeFunctionCall9 = myAssertEqual "getTypeFunctionCall with %"
    (T_Function [typeInteger, typeInteger] typeInteger)
    (getTypeFunctionCall "%" [])

testGetTypeFunctionCall10 :: Test
testGetTypeFunctionCall10 = myAssertEqual "getTypeFunctionCall with mod"
    (T_Function [typeInteger, typeInteger] typeInteger)
    (getTypeFunctionCall "mod" [])

testGetTypeFunctionCall11 :: Test
testGetTypeFunctionCall11 = myAssertEqual "getTypeFunctionCall with v-"
    (T_Function [typeNumber] T_Float)
    (getTypeFunctionCall "v-" [])

testGetTypeFunctionCall12 :: Test
testGetTypeFunctionCall12 = myAssertEqual "getTypeFunctionCall with sqrt"
    (T_Function [typeNumber] T_Float)
    (getTypeFunctionCall "sqrt" [])

testGetTypeFunctionCall13 :: Test
testGetTypeFunctionCall13 = myAssertEqual "getTypeFunctionCall with !!"
    (T_Function [typeInteger] T_UInt)
    (getTypeFunctionCall "!!" [])

testGetTypeFunctionCall14 :: Test
testGetTypeFunctionCall14 = myAssertEqual "getTypeFunctionCall with factorial"
    (T_Function [typeInteger] T_UInt)
    (getTypeFunctionCall "factorial" [])

testGetTypeFunctionCall15 :: Test
testGetTypeFunctionCall15 = myAssertEqual "getTypeFunctionCall with +="
    (T_Function [T_String, typeNumber] T_Bool)
    (getTypeFunctionCall "+=" [])

testGetTypeFunctionCall16 :: Test
testGetTypeFunctionCall16 = myAssertEqual "getTypeFunctionCall with add="
    (T_Function [T_String, typeNumber] T_Bool)
    (getTypeFunctionCall "add=" [])

testGetTypeFunctionCall17 :: Test
testGetTypeFunctionCall17 = myAssertEqual "getTypeFunctionCall with -="
    (T_Function [T_String, typeNumber] T_Bool)
    (getTypeFunctionCall "-=" [])

testGetTypeFunctionCall18 :: Test
testGetTypeFunctionCall18 = myAssertEqual "getTypeFunctionCall with sub="
    (T_Function [T_String, typeNumber] T_Bool)
    (getTypeFunctionCall "sub=" [])

testGetTypeFunctionCall19 :: Test
testGetTypeFunctionCall19 = myAssertEqual "getTypeFunctionCall with *="
    (T_Function [T_String, typeNumber] T_Bool)
    (getTypeFunctionCall "*=" [])

testGetTypeFunctionCall20 :: Test
testGetTypeFunctionCall20 = myAssertEqual "getTypeFunctionCall with mul="
    (T_Function [T_String, typeNumber] T_Bool)
    (getTypeFunctionCall "mul=" [])

testGetTypeFunctionCall21 :: Test
testGetTypeFunctionCall21 = myAssertEqual "getTypeFunctionCall with /="
    (T_Function [T_String, typeNumber] T_Bool)
    (getTypeFunctionCall "/=" [])

testGetTypeFunctionCall22 :: Test
testGetTypeFunctionCall22 = myAssertEqual "getTypeFunctionCall with div="
    (T_Function [T_String, typeNumber] T_Bool)
    (getTypeFunctionCall "div=" [])

testGetTypeFunctionCall23 :: Test
testGetTypeFunctionCall23 = myAssertEqual "getTypeFunctionCall with %="
    (T_Function [T_String, typeNumber] T_Bool)
    (getTypeFunctionCall "%=" [])

testGetTypeFunctionCall24 :: Test
testGetTypeFunctionCall24 = myAssertEqual "getTypeFunctionCall with mod="
    (T_Function [T_String, typeNumber] T_Bool)
    (getTypeFunctionCall "mod=" [])

testGetTypeFunctionCall25 :: Test
testGetTypeFunctionCall25 = myAssertEqual "getTypeFunctionCall with **="
    (T_Function [T_String, typeNumber] T_Bool)
    (getTypeFunctionCall "**=" [])

testGetTypeFunctionCall26 :: Test
testGetTypeFunctionCall26 = myAssertEqual "getTypeFunctionCall with pow="
    (T_Function [T_String, typeNumber] T_Bool)
    (getTypeFunctionCall "pow=" [])

testGetTypeFunctionCall27 :: Test
testGetTypeFunctionCall27 = myAssertEqual "getTypeFunctionCall with =="
    (T_Function [typeAny, typeAny] T_Bool)
    (getTypeFunctionCall "==" [])

testGetTypeFunctionCall28 :: Test
testGetTypeFunctionCall28 = myAssertEqual "getTypeFunctionCall with eq"
    (T_Function [typeAny, typeAny] T_Bool)
    (getTypeFunctionCall "eq" [])

testGetTypeFunctionCall29 :: Test
testGetTypeFunctionCall29 = myAssertEqual "getTypeFunctionCall with !="
    (T_Function [typeAny, typeAny] T_Bool)
    (getTypeFunctionCall "!=" [])

testGetTypeFunctionCall30 :: Test
testGetTypeFunctionCall30 = myAssertEqual "getTypeFunctionCall with neq"
    (T_Function [typeAny, typeAny] T_Bool)
    (getTypeFunctionCall "neq" [])

testGetTypeFunctionCall31 :: Test
testGetTypeFunctionCall31 = myAssertEqual "getTypeFunctionCall with <"
    (T_Function [typeNumber, typeNumber] T_Bool)
    (getTypeFunctionCall "<" [])

testGetTypeFunctionCall32 :: Test
testGetTypeFunctionCall32 = myAssertEqual "getTypeFunctionCall with lw"
    (T_Function [typeNumber, typeNumber] T_Bool)
    (getTypeFunctionCall "lw" [])

testGetTypeFunctionCall33 :: Test
testGetTypeFunctionCall33 = myAssertEqual "getTypeFunctionCall with >"
    (T_Function [typeNumber, typeNumber] T_Bool)
    (getTypeFunctionCall ">" [])

testGetTypeFunctionCall34 :: Test
testGetTypeFunctionCall34 = myAssertEqual "getTypeFunctionCall with gt"
    (T_Function [typeNumber, typeNumber] T_Bool)
    (getTypeFunctionCall "gt" [])

testGetTypeFunctionCall35 :: Test
testGetTypeFunctionCall35 = myAssertEqual "getTypeFunctionCall with <="
    (T_Function [typeNumber, typeNumber] T_Bool)
    (getTypeFunctionCall "<=" [])

testGetTypeFunctionCall36 :: Test
testGetTypeFunctionCall36 = myAssertEqual "getTypeFunctionCall with lweq"
    (T_Function [typeNumber, typeNumber] T_Bool)
    (getTypeFunctionCall "lweq" [])

testGetTypeFunctionCall37 :: Test
testGetTypeFunctionCall37 = myAssertEqual "getTypeFunctionCall with >="
    (T_Function [typeNumber, typeNumber] T_Bool)
    (getTypeFunctionCall ">=" [])

testGetTypeFunctionCall38 :: Test
testGetTypeFunctionCall38 = myAssertEqual "getTypeFunctionCall with gteq"
    (T_Function [typeNumber, typeNumber] T_Bool)
    (getTypeFunctionCall "gteq" [])

testGetTypeFunctionCall39 :: Test
testGetTypeFunctionCall39 = myAssertEqual "getTypeFunctionCall with !"
    (T_Function [T_Bool] T_Bool)
    (getTypeFunctionCall "!" [])

testGetTypeFunctionCall40 :: Test
testGetTypeFunctionCall40 = myAssertEqual "getTypeFunctionCall with not"
    (T_Function [T_Bool] T_Bool)
    (getTypeFunctionCall "not" [])

testGetTypeFunctionCall41 :: Test
testGetTypeFunctionCall41 = myAssertEqual "getTypeFunctionCall with &&"
    (T_Function [T_Bool, T_Bool] T_Bool)
    (getTypeFunctionCall "&&" [])

testGetTypeFunctionCall42 :: Test
testGetTypeFunctionCall42 = myAssertEqual "getTypeFunctionCall with and"
    (T_Function [T_Bool, T_Bool] T_Bool)
    (getTypeFunctionCall "and" [])

testGetTypeFunctionCall43 :: Test
testGetTypeFunctionCall43 = myAssertEqual "getTypeFunctionCall with ||"
    (T_Function [T_Bool, T_Bool] T_Bool)
    (getTypeFunctionCall "||" [])

testGetTypeFunctionCall44 :: Test
testGetTypeFunctionCall44 = myAssertEqual "getTypeFunctionCall with or"
    (T_Function [T_Bool, T_Bool] T_Bool)
    (getTypeFunctionCall "or" [])

testGetTypeFunctionCall45 :: Test
testGetTypeFunctionCall45 = myAssertEqual "getTypeFunctionCall with !&"
    (T_Function [T_Bool, T_Bool] T_Bool)
    (getTypeFunctionCall "!&" [])

testGetTypeFunctionCall46 :: Test
testGetTypeFunctionCall46 = myAssertEqual "getTypeFunctionCall with nand"
    (T_Function [T_Bool, T_Bool] T_Bool)
    (getTypeFunctionCall "nand" [])

testGetTypeFunctionCall47 :: Test
testGetTypeFunctionCall47 = myAssertEqual "getTypeFunctionCall with !|"
    (T_Function [T_Bool, T_Bool] T_Bool)
    (getTypeFunctionCall "!|" [])

testGetTypeFunctionCall48 :: Test
testGetTypeFunctionCall48 = myAssertEqual "getTypeFunctionCall with nor"
    (T_Function [T_Bool, T_Bool] T_Bool)
    (getTypeFunctionCall "nor" [])

testGetTypeFunctionCall49 :: Test
testGetTypeFunctionCall49 = myAssertEqual "getTypeFunctionCall with :|"
    (T_Function [T_Bool, T_Bool] T_Bool)
    (getTypeFunctionCall ":|" [])

testGetTypeFunctionCall50 :: Test
testGetTypeFunctionCall50 = myAssertEqual "getTypeFunctionCall with xor"
    (T_Function [T_Bool, T_Bool] T_Bool)
    (getTypeFunctionCall "xor" [])

testGetTypeFunctionCall51 :: Test
testGetTypeFunctionCall51 = myAssertEqual "getTypeFunctionCall with !:"
    (T_Function [T_Bool, T_Bool] T_Bool)
    (getTypeFunctionCall "!:" [])

testGetTypeFunctionCall52 :: Test
testGetTypeFunctionCall52 = myAssertEqual "getTypeFunctionCall with xnor"
    (T_Function [T_Bool, T_Bool] T_Bool)
    (getTypeFunctionCall "xnor" [])

testGetTypeFunctionCall53 :: Test
testGetTypeFunctionCall53 = myAssertEqual "getTypeFunctionCall with &"
    (T_Function [typeInteger, typeInteger] typeInteger)
    (getTypeFunctionCall "&" [])

testGetTypeFunctionCall54 :: Test
testGetTypeFunctionCall54 = myAssertEqual "getTypeFunctionCall with band"
    (T_Function [typeInteger, typeInteger] typeInteger)
    (getTypeFunctionCall "band" [])

testGetTypeFunctionCall55 :: Test
testGetTypeFunctionCall55 = myAssertEqual "getTypeFunctionCall with |"
    (T_Function [typeInteger, typeInteger] typeInteger)
    (getTypeFunctionCall "|" [])

testGetTypeFunctionCall56 :: Test
testGetTypeFunctionCall56 = myAssertEqual "getTypeFunctionCall with bor"
    (T_Function [typeInteger, typeInteger] typeInteger)
    (getTypeFunctionCall "bor" [])

testGetTypeFunctionCall57 :: Test
testGetTypeFunctionCall57 = myAssertEqual "getTypeFunctionCall with ~"
    (T_Function [typeInteger, typeInteger] typeInteger)
    (getTypeFunctionCall "~" [])

testGetTypeFunctionCall58 :: Test
testGetTypeFunctionCall58 = myAssertEqual "getTypeFunctionCall with bnot"
    (T_Function [typeInteger, typeInteger] typeInteger)
    (getTypeFunctionCall "bnot" [])

testGetTypeFunctionCall59 :: Test
testGetTypeFunctionCall59 = myAssertEqual "getTypeFunctionCall with ^"
    (T_Function [typeInteger, typeInteger] typeInteger)
    (getTypeFunctionCall "^" [])

testGetTypeFunctionCall60 :: Test
testGetTypeFunctionCall60 = myAssertEqual "getTypeFunctionCall with bxor"
    (T_Function [typeInteger, typeInteger] typeInteger)
    (getTypeFunctionCall "bxor" [])

testGetTypeFunctionCall61 :: Test
testGetTypeFunctionCall61 = myAssertEqual "getTypeFunctionCall with <<"
    (T_Function [typeInteger, typeInteger] typeInteger)
    (getTypeFunctionCall "<<" [])

testGetTypeFunctionCall62 :: Test
testGetTypeFunctionCall62 = myAssertEqual "getTypeFunctionCall with lshift"
    (T_Function [typeInteger, typeInteger] typeInteger)
    (getTypeFunctionCall "lshift" [])

testGetTypeFunctionCall63 :: Test
testGetTypeFunctionCall63 = myAssertEqual "getTypeFunctionCall with >>"
    (T_Function [typeInteger, typeInteger] typeInteger)
    (getTypeFunctionCall ">>" [])

testGetTypeFunctionCall64 :: Test
testGetTypeFunctionCall64 = myAssertEqual "getTypeFunctionCall with rshift"
    (T_Function [typeInteger, typeInteger] typeInteger)
    (getTypeFunctionCall "rshift" [])

testGetTypeFunctionCall65 :: Test
testGetTypeFunctionCall65 = myAssertEqual "getTypeFunctionCall with left"
    (T_Function [T_Tuple (T_Template, T_Template)] T_Template)
    (getTypeFunctionCall "left" [])

testGetTypeFunctionCall66 :: Test
testGetTypeFunctionCall66 = myAssertEqual "getTypeFunctionCall with right"
    (T_Function [T_Tuple (T_Template, T_Template)] T_Template)
    (getTypeFunctionCall "right" [])

testGetTypeFunctionCall67 :: Test
testGetTypeFunctionCall67 = myAssertEqual "getTypeFunctionCall with swap"
    (T_Function [T_Tuple (T_Template, T_Template)] (T_Tuple (T_Template, T_Template)))
    (getTypeFunctionCall "swap" [])

testGetTypeFunctionCall68 :: Test
testGetTypeFunctionCall68 = myAssertEqual "getTypeFunctionCall with len"
    (T_Function [T_List T_Template] T_UInt)
    (getTypeFunctionCall "len" [])

testGetTypeFunctionCall69 :: Test
testGetTypeFunctionCall69 = myAssertEqual "getTypeFunctionCall with length"
    (T_Function [T_List T_Template] T_UInt)
    (getTypeFunctionCall "length" [])

testGetTypeFunctionCall70 :: Test
testGetTypeFunctionCall70 = myAssertEqual "getTypeFunctionCall with concat"
    (T_Function [T_List T_Template, T_List T_Template] (T_List T_Template))
    (getTypeFunctionCall "concat" [])

testGetTypeFunctionCall71 :: Test
testGetTypeFunctionCall71 = myAssertEqual "getTypeFunctionCall with split"
    (T_Function [T_List T_Template, T_UInt] (T_Tuple (T_List T_Template, T_List T_Template)))
    (getTypeFunctionCall "split" [])

testGetTypeFunctionCall72 :: Test
testGetTypeFunctionCall72 = myAssertEqual "getTypeFunctionCall with first"
    (T_Function [T_List T_Template] T_Template)
    (getTypeFunctionCall "first" [])

testGetTypeFunctionCall73 :: Test
testGetTypeFunctionCall73 = myAssertEqual "getTypeFunctionCall with last"
    (T_Function [T_List T_Template] T_Template)
    (getTypeFunctionCall "last" [])

testGetTypeFunctionCall74 :: Test
testGetTypeFunctionCall74 = myAssertEqual "getTypeFunctionCall with pushback"
    (T_Function [T_List T_Template, T_Template] (T_List T_Template))
    (getTypeFunctionCall "pushback" [])

testGetTypeFunctionCall75 :: Test
testGetTypeFunctionCall75 = myAssertEqual "getTypeFunctionCall with pushfront"
    (T_Function [T_List T_Template, T_Template] (T_List T_Template))
    (getTypeFunctionCall "pushfront" [])

testGetTypeFunctionCall76 :: Test
testGetTypeFunctionCall76 = myAssertEqual "getTypeFunctionCall with get"
    (T_Function [T_List T_Template, T_UInt] T_Template)
    (getTypeFunctionCall "get" [])

testGetTypeFunctionCall77 :: Test
testGetTypeFunctionCall77 = myAssertEqual "getTypeFunctionCall with reverse"
    (T_Function [T_List T_Template] (T_List T_Template))
    (getTypeFunctionCall "reverse" [])

testGetTypeFunctionCall78 :: Test
testGetTypeFunctionCall78 = myAssertEqual "getTypeFunctionCall with exp"
    (T_Function [typeNumber] T_Float)
    (getTypeFunctionCall "exp" [])

testGetTypeFunctionCall79 :: Test
testGetTypeFunctionCall79 = myAssertEqual "getTypeFunctionCall with ln"
    (T_Function [typeNumber] T_Float)
    (getTypeFunctionCall "ln" [])

testGetTypeFunctionCall80 :: Test
testGetTypeFunctionCall80 = myAssertEqual "getTypeFunctionCall with max"
    (T_Function [typeNumber, typeNumber] typeNumber)
    (getTypeFunctionCall "max" [])

testGetTypeFunctionCall81 :: Test
testGetTypeFunctionCall81 = myAssertEqual "getTypeFunctionCall with min"
    (T_Function [typeNumber, typeNumber] typeNumber)
    (getTypeFunctionCall "min" [])

testGetTypeFunctionCall82 :: Test
testGetTypeFunctionCall82 = myAssertEqual "getTypeFunctionCall with cos"
    (T_Function [typeNumber] T_Float)
    (getTypeFunctionCall "cos" [])

testGetTypeFunctionCall83 :: Test
testGetTypeFunctionCall83 = myAssertEqual "getTypeFunctionCall with acos"
    (T_Function [typeNumber] T_Float)
    (getTypeFunctionCall "acos" [])

testGetTypeFunctionCall84 :: Test
testGetTypeFunctionCall84 = myAssertEqual "getTypeFunctionCall with cosh"
    (T_Function [typeNumber] T_Float)
    (getTypeFunctionCall "cosh" [])

testGetTypeFunctionCall85 :: Test
testGetTypeFunctionCall85 = myAssertEqual "getTypeFunctionCall with sin"
    (T_Function [typeNumber] T_Float)
    (getTypeFunctionCall "sin" [])

testGetTypeFunctionCall86 :: Test
testGetTypeFunctionCall86 = myAssertEqual "getTypeFunctionCall with asin"
    (T_Function [typeNumber] T_Float)
    (getTypeFunctionCall "asin" [])

testGetTypeFunctionCall87 :: Test
testGetTypeFunctionCall87 = myAssertEqual "getTypeFunctionCall with sinh"
    (T_Function [typeNumber] T_Float)
    (getTypeFunctionCall "sinh" [])

testGetTypeFunctionCall88 :: Test
testGetTypeFunctionCall88 = myAssertEqual "getTypeFunctionCall with tan"
    (T_Function [typeNumber] T_Float)
    (getTypeFunctionCall "tan" [])

testGetTypeFunctionCall89 :: Test
testGetTypeFunctionCall89 = myAssertEqual "getTypeFunctionCall with atan"
    (T_Function [typeNumber] T_Float)
    (getTypeFunctionCall "atan" [])

testGetTypeFunctionCall90 :: Test
testGetTypeFunctionCall90 = myAssertEqual "getTypeFunctionCall with tanh"
    (T_Function [typeNumber] T_Float)
    (getTypeFunctionCall "tanh" [])

testGetTypeFunctionCall91 :: Test
testGetTypeFunctionCall91 = myAssertEqual "getTypeFunctionCall with ceil"
    (T_Function [T_Float] T_Float)
    (getTypeFunctionCall "ceil" [])

testGetTypeFunctionCall92 :: Test
testGetTypeFunctionCall92 = myAssertEqual "getTypeFunctionCall with round"
    (T_Function [T_Float] T_Float)
    (getTypeFunctionCall "round" [])

testGetTypeFunctionCall93 :: Test
testGetTypeFunctionCall93 = myAssertEqual "getTypeFunctionCall with trunc"
    (T_Function [T_Float] T_Float)
    (getTypeFunctionCall "trunc" [])

testGetTypeFunctionCall94 :: Test
testGetTypeFunctionCall94 = myAssertEqual "getTypeFunctionCall with floor"
    (T_Function [T_Float] T_Float)
    (getTypeFunctionCall "floor" [])

testGetTypeFunctionCall95 :: Test
testGetTypeFunctionCall95 = myAssertEqual "getTypeFunctionCall with undefined procedure"
    T_Undefined
    (getTypeFunctionCall "undefinedProcedure" [])

testGetTypeFunctionCall96 :: Test
testGetTypeFunctionCall96 = myAssertEqual "getTypeFunctionCall with matching ASTDefine"
    (T_Function [typeNumber] T_Float)
    (getTypeFunctionCall "sqrt" [ASTDefine "sqrt" (T_Function [typeNumber] T_Float) (ASTFloat 1.0)])

testGetTypeFunctionCall97 :: Test
testGetTypeFunctionCall97 = myAssertEqual "getTypeFunctionCall with matching ASTFunction"
    (T_Function [typeNumber] T_Float)
    (getTypeFunctionCall "sqrt" [ASTFunction "sqrt" [(ASTProcedure "x", typeNumber)] (ASTFloat 1.0) T_Float])

testGetTypeFunctionCall98 :: Test
testGetTypeFunctionCall98 = myAssertEqual "getTypeFunctionCall with mismatched procedure name"
    T_Undefined
    (getTypeFunctionCall "nonExistent" [ASTFunction "sqrt" [(ASTProcedure "x", typeNumber)] (ASTFloat 1.0) T_Float])

testGetTypeFunctionCall :: Test
testGetTypeFunctionCall = TestList [
    TestLabel "getTypeFunctionCall with +" testGetTypeFunctionCall1,
    TestLabel "getTypeFunctionCall with add" testGetTypeFunctionCall2,
    TestLabel "getTypeFunctionCall with -" testGetTypeFunctionCall3,
    TestLabel "getTypeFunctionCall with sub" testGetTypeFunctionCall4,
    TestLabel "getTypeFunctionCall with *" testGetTypeFunctionCall5,
    TestLabel "getTypeFunctionCall with mul" testGetTypeFunctionCall6,
    TestLabel "getTypeFunctionCall with /" testGetTypeFunctionCall7,
    TestLabel "getTypeFunctionCall with div" testGetTypeFunctionCall8,
    TestLabel "getTypeFunctionCall with %" testGetTypeFunctionCall9,
    TestLabel "getTypeFunctionCall with mod" testGetTypeFunctionCall10,
    TestLabel "getTypeFunctionCall with v-" testGetTypeFunctionCall11,
    TestLabel "getTypeFunctionCall with sqrt" testGetTypeFunctionCall12,
    TestLabel "getTypeFunctionCall with !!" testGetTypeFunctionCall13,
    TestLabel "getTypeFunctionCall with factorial" testGetTypeFunctionCall14,
    TestLabel "getTypeFunctionCall with +=" testGetTypeFunctionCall15,
    TestLabel "getTypeFunctionCall with add=" testGetTypeFunctionCall16,
    TestLabel "getTypeFunctionCall with -=" testGetTypeFunctionCall17,
    TestLabel "getTypeFunctionCall with sub=" testGetTypeFunctionCall18,
    TestLabel "getTypeFunctionCall with *=" testGetTypeFunctionCall19,
    TestLabel "getTypeFunctionCall with mul=" testGetTypeFunctionCall20,
    TestLabel "getTypeFunctionCall with /=" testGetTypeFunctionCall21,
    TestLabel "getTypeFunctionCall with div=" testGetTypeFunctionCall22,
    TestLabel "getTypeFunctionCall with %=" testGetTypeFunctionCall23,
    TestLabel "getTypeFunctionCall with mod=" testGetTypeFunctionCall24,
    TestLabel "getTypeFunctionCall with **=" testGetTypeFunctionCall25,
    TestLabel "getTypeFunctionCall with pow=" testGetTypeFunctionCall26,
    TestLabel "getTypeFunctionCall with ==" testGetTypeFunctionCall27,
    TestLabel "getTypeFunctionCall with eq" testGetTypeFunctionCall28,
    TestLabel "getTypeFunctionCall with !=" testGetTypeFunctionCall29,
    TestLabel "getTypeFunctionCall with neq" testGetTypeFunctionCall30,
    TestLabel "getTypeFunctionCall with <" testGetTypeFunctionCall31,
    TestLabel "getTypeFunctionCall with lw" testGetTypeFunctionCall32,
    TestLabel "getTypeFunctionCall with >" testGetTypeFunctionCall33,
    TestLabel "getTypeFunctionCall with gt" testGetTypeFunctionCall34,
    TestLabel "getTypeFunctionCall with <=" testGetTypeFunctionCall35,
    TestLabel "getTypeFunctionCall with lweq" testGetTypeFunctionCall36,
    TestLabel "getTypeFunctionCall with >=" testGetTypeFunctionCall37,
    TestLabel "getTypeFunctionCall with gteq" testGetTypeFunctionCall38,
    TestLabel "getTypeFunctionCall with !" testGetTypeFunctionCall39,
    TestLabel "getTypeFunctionCall with not" testGetTypeFunctionCall40,
    TestLabel "getTypeFunctionCall with &&" testGetTypeFunctionCall41,
    TestLabel "getTypeFunctionCall with and" testGetTypeFunctionCall42,
    TestLabel "getTypeFunctionCall with ||" testGetTypeFunctionCall43,
    TestLabel "getTypeFunctionCall with or" testGetTypeFunctionCall44,
    TestLabel "getTypeFunctionCall with !&" testGetTypeFunctionCall45,
    TestLabel "getTypeFunctionCall with nand" testGetTypeFunctionCall46,
    TestLabel "getTypeFunctionCall with !|" testGetTypeFunctionCall47,
    TestLabel "getTypeFunctionCall with nor" testGetTypeFunctionCall48,
    TestLabel "getTypeFunctionCall with :|" testGetTypeFunctionCall49,
    TestLabel "getTypeFunctionCall with xor" testGetTypeFunctionCall50,
    TestLabel "getTypeFunctionCall with !:" testGetTypeFunctionCall51,
    TestLabel "getTypeFunctionCall with xnor" testGetTypeFunctionCall52,
    TestLabel "getTypeFunctionCall with &" testGetTypeFunctionCall53,
    TestLabel "getTypeFunctionCall with band" testGetTypeFunctionCall54,
    TestLabel "getTypeFunctionCall with |" testGetTypeFunctionCall55,
    TestLabel "getTypeFunctionCall with bor" testGetTypeFunctionCall56,
    TestLabel "getTypeFunctionCall with ~" testGetTypeFunctionCall57,
    TestLabel "getTypeFunctionCall with bnot" testGetTypeFunctionCall58,
    TestLabel "getTypeFunctionCall with ^" testGetTypeFunctionCall59,
    TestLabel "getTypeFunctionCall with bxor" testGetTypeFunctionCall60,
    TestLabel "getTypeFunctionCall with <<" testGetTypeFunctionCall61,
    TestLabel "getTypeFunctionCall with lshift" testGetTypeFunctionCall62,
    TestLabel "getTypeFunctionCall with >>" testGetTypeFunctionCall63,
    TestLabel "getTypeFunctionCall with rshift" testGetTypeFunctionCall64,
    TestLabel "getTypeFunctionCall with left" testGetTypeFunctionCall65,
    TestLabel "getTypeFunctionCall with right" testGetTypeFunctionCall66,
    TestLabel "getTypeFunctionCall with swap" testGetTypeFunctionCall67,
    TestLabel "getTypeFunctionCall with len" testGetTypeFunctionCall68,
    TestLabel "getTypeFunctionCall with length" testGetTypeFunctionCall69,
    TestLabel "getTypeFunctionCall with concat" testGetTypeFunctionCall70,
    TestLabel "getTypeFunctionCall with split" testGetTypeFunctionCall71,
    TestLabel "getTypeFunctionCall with first" testGetTypeFunctionCall72,
    TestLabel "getTypeFunctionCall with last" testGetTypeFunctionCall73,
    TestLabel "getTypeFunctionCall with pushback" testGetTypeFunctionCall74,
    TestLabel "getTypeFunctionCall with pushfront" testGetTypeFunctionCall75,
    TestLabel "getTypeFunctionCall with get" testGetTypeFunctionCall76,
    TestLabel "getTypeFunctionCall with reverse" testGetTypeFunctionCall77,
    TestLabel "getTypeFunctionCall with exp" testGetTypeFunctionCall78,
    TestLabel "getTypeFunctionCall with ln" testGetTypeFunctionCall79,
    TestLabel "getTypeFunctionCall with max" testGetTypeFunctionCall80,
    TestLabel "getTypeFunctionCall with min" testGetTypeFunctionCall81,
    TestLabel "getTypeFunctionCall with cos" testGetTypeFunctionCall82,
    TestLabel "getTypeFunctionCall with acos" testGetTypeFunctionCall83,
    TestLabel "getTypeFunctionCall with cosh" testGetTypeFunctionCall84,
    TestLabel "getTypeFunctionCall with sin" testGetTypeFunctionCall85,
    TestLabel "getTypeFunctionCall with asin" testGetTypeFunctionCall86,
    TestLabel "getTypeFunctionCall with sinh" testGetTypeFunctionCall87,
    TestLabel "getTypeFunctionCall with tan" testGetTypeFunctionCall88,
    TestLabel "getTypeFunctionCall with atan" testGetTypeFunctionCall89,
    TestLabel "getTypeFunctionCall with tanh" testGetTypeFunctionCall90,
    TestLabel "getTypeFunctionCall with ceil" testGetTypeFunctionCall91,
    TestLabel "getTypeFunctionCall with round" testGetTypeFunctionCall92,
    TestLabel "getTypeFunctionCall with trunc" testGetTypeFunctionCall93,
    TestLabel "getTypeFunctionCall with floor" testGetTypeFunctionCall94,
    TestLabel "getTypeFunctionCall with undefined procedure" testGetTypeFunctionCall95,
    TestLabel "getTypeFunctionCall with matching ASTDefine" testGetTypeFunctionCall96,
    TestLabel "getTypeFunctionCall with matching ASTFunction" testGetTypeFunctionCall97,
    TestLabel "getTypeFunctionCall with mismatched procedure name" testGetTypeFunctionCall98
    ]

-------------------------------------------------------------------------------

testAST :: Test
testAST = TestList [
    TestLabel "AST deriving Eq" testASTEquality,
    TestLabel "AST deriving Show" testShowAST,
    TestLabel "getTypeFunctionCall" testGetTypeFunctionCall
    ]
