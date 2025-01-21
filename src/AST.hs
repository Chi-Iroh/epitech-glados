module AST (
    Parameter,
    Call(..),
    AST(..),
    MainAST(..),
    getTypeFunctionCall,
    getTypeAST,
    getTypeAST',
    isProcedureType
    ) where

import Type

type Parameter = (AST, Type)

data Call = LambdaCall [Parameter] AST Type | FunctionCall String deriving (Eq, Show)

-- ASTBoolean devient ASTBool ; ASTSymbol devient ASTProcedure
data AST =  ASTInt Int                              |
            ASTUInt Int                             |
            ASTChar Char                            |
            ASTFloat Float                          |
            ASTBool Bool                            |
            ASTTuple (AST, AST)                     |
            ASTArray [AST]                          |
            -- ASTList [AST]                           |
            ASTString String                        |
            ASTProcedure String                     |
            ASTDefine String Type AST               |
            ASTFunction String [Parameter] AST Type |
            ASTLambda [Parameter] AST Type          |
            ASTCall Call [AST]                      |
            ASTIf AST AST AST                       |
            ASTNULL deriving (Eq, Show)

data MainAST = MainAST AST

getTypeProcedure :: String -> [AST] -> Type
getTypeProcedure "e" _ = T_Float
getTypeProcedure "pi" _ = T_Float
getTypeProcedure _ [] = T_Undefined
getTypeProcedure procedure (ASTDefine name t _:rest)
    | procedure == name = t
    | otherwise = getTypeProcedure procedure rest
getTypeProcedure procedure (_:rest) = getTypeProcedure procedure rest

getTypeFunctionCall :: String -> [AST] -> Type
getTypeFunctionCall "+" _ = T_Function [typeNumber, typeNumber] typeNumber
getTypeFunctionCall "add" _ = T_Function [typeNumber, typeNumber] typeNumber
getTypeFunctionCall "-" _ = T_Function [typeNumber, typeNumber] typeNumber
getTypeFunctionCall "sub" _ = T_Function [typeNumber, typeNumber] typeNumber
getTypeFunctionCall "*" _ = T_Function [typeNumber, typeNumber] typeNumber
getTypeFunctionCall "mul" _ = T_Function [typeNumber, typeNumber] typeNumber
getTypeFunctionCall "/" _ = T_Function [typeNumber, typeNumber] typeNumber
getTypeFunctionCall "div" _ = T_Function [typeNumber, typeNumber] typeNumber
getTypeFunctionCall "%" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "mod" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "v-" _ = T_Function [typeNumber] T_Float
getTypeFunctionCall "sqrt" _ = T_Function [typeNumber] T_Float
getTypeFunctionCall "!!" _ = T_Function [typeInteger] T_UInt
getTypeFunctionCall "factorial" _ = T_Function [typeInteger] T_UInt
getTypeFunctionCall "+=" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "add=" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "-=" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "sub=" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "*=" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "mul=" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "/=" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "div=" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "%=" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "mod=" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "**=" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "pow=" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "==" _ = T_Function [typeAny, typeAny] T_Bool
getTypeFunctionCall "eq" _ = T_Function [typeAny, typeAny] T_Bool
getTypeFunctionCall "!=" _ = T_Function [typeAny, typeAny] T_Bool
getTypeFunctionCall "<" _ = T_Function [typeNumber, typeNumber] T_Bool
getTypeFunctionCall ">" _ = T_Function [typeNumber, typeNumber] T_Bool
getTypeFunctionCall "<=" _ = T_Function [typeNumber, typeNumber] T_Bool
getTypeFunctionCall ">=" _ = T_Function [typeNumber, typeNumber] T_Bool
getTypeFunctionCall "!" _ = T_Function [T_Bool] T_Bool
getTypeFunctionCall "not" _ = T_Function [T_Bool] T_Bool
getTypeFunctionCall "&&" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "and" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "||" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "or" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "!&" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "nand" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "!|" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "nor" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall ":|" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "xor" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "!:" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "xnor" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "&" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "|" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "~" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "^" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "<<" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall ">>" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall _ [] = T_Undefined
getTypeFunctionCall procedure (ASTDefine name t@(T_Function _ _) _:rest)
    | procedure == name = t
    | otherwise = getTypeFunctionCall procedure rest
getTypeFunctionCall procedure (ASTFunction name parameters _ r:rest)
    | procedure == name = T_Function (map snd parameters) r
    | otherwise = getTypeFunctionCall procedure rest
getTypeFunctionCall procedure (_:rest) = getTypeProcedure procedure rest

getReturnType :: Type -> Type
getReturnType (T_Function _ t) = t
getReturnType _ = T_Undefined

getTypeAST :: AST -> [AST] -> Type
getTypeAST (ASTInt _) _ = T_Int
getTypeAST (ASTUInt _) _ = T_UInt
getTypeAST (ASTChar _) _ = T_Char
getTypeAST (ASTFloat _) _ = T_Float
getTypeAST (ASTBool _) _ = T_Bool
getTypeAST (ASTTuple (a, b)) tt = T_Tuple ((getTypeAST a tt), (getTypeAST b tt))
getTypeAST (ASTString _) _ = T_String
getTypeAST (ASTProcedure procedure) tt = getTypeProcedure procedure tt
getTypeAST (ASTDefine _ _ _) _ = T_Procedure
getTypeAST (ASTFunction _ parameters _ r) _ = T_Function (map snd parameters) r
getTypeAST (ASTLambda parameters _ r) _ = T_Function (map snd parameters) r
getTypeAST (ASTCall (LambdaCall a b c) _) tt = getReturnType $ getTypeAST (ASTLambda a b c) tt
getTypeAST (ASTCall (FunctionCall a) _) tt = getReturnType $ getTypeFunctionCall a tt
getTypeAST (ASTIf _ a b) tt = T_Combination ((getTypeAST a tt):(getTypeAST b tt):[])
getTypeAST (ASTArray list) tt = verifyTypeList $ map (\x -> getTypeAST x tt) list
getTypeAST ASTNULL _ = T_NULL

getTypeAST' :: AST -> Type
getTypeAST' ast = getTypeAST ast []

isProcedureType :: MainAST -> Bool
isProcedureType (MainAST (ASTLambda _ _ _)) = True
isProcedureType (MainAST (ASTDefine _ _ _)) = True
isProcedureType _ = False

instance Show MainAST where
    show (MainAST (ASTInt n)) = show n
    show (MainAST (ASTBool b)) = if b then "#t" else "#f"
    show _ = ""
