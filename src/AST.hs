module AST (
    Parameter,
    Call(..),
    AST(..),
    MainAST(..),
    getTypeAST,
    isProcedureType
    ) where

import Type
import Utils

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
            ASTString String                        |
            ASTProcedure String                     |
            ASTDefine String Type AST               |
            ASTFunction String [Parameter] AST Type |
            ASTLambda [Parameter] AST Type          |
            ASTCall Call [AST]                      |
            ASTIf AST AST AST                       |
            ASTNULL
            deriving (Eq, Show)

data MainAST = MainAST AST

getReturnType :: Safe Type -> Safe Type
getReturnType (Error err) = Error err
getReturnType (Value (T_Function _ t)) = Value t
getReturnType _ = Error "Glados: TypeError: Mixed types within a list."

getTypeAST :: AST -> Safe Type
getTypeAST (ASTInt _) = Value T_Int
getTypeAST (ASTUInt _) = Value T_UInt
getTypeAST (ASTChar _) = Value T_Char
getTypeAST (ASTFloat _) = Value T_Float
getTypeAST (ASTBool _) = Value T_Bool
getTypeAST (ASTTuple (a, b)) = verifyTypeTuple ((getTypeAST a), (getTypeAST b))
getTypeAST (ASTString _) = Value T_String
getTypeAST (ASTProcedure _) = Value T_Procedure
getTypeAST (ASTDefine _ _ _) = Value T_Procedure
getTypeAST (ASTFunction _ parameters _ r) = Value $ T_Function (map snd parameters) r
getTypeAST (ASTLambda parameters _ r) = Value $ T_Function (map snd parameters) r
getTypeAST (ASTCall (LambdaCall a b c) _) = getReturnType $ getTypeAST $ ASTLambda a b c
getTypeAST (ASTCall (FunctionCall _) _) = Value T_Undefined
getTypeAST (ASTIf _ a b) = combinateTypes ((getTypeAST a):(getTypeAST b):[])
getTypeAST (ASTArray []) = Value T_EmptyList
getTypeAST (ASTArray [x]) = verifyTypeList [getTypeAST x]
getTypeAST (ASTArray list) = verifyTypeList $ map getTypeAST list
getTypeAST ASTNULL = Value T_NULL

isProcedureType :: MainAST -> Bool
isProcedureType (MainAST (ASTLambda _ _ _)) = True
isProcedureType (MainAST (ASTDefine _ _ _)) = True
isProcedureType _ = False

instance Show MainAST where
    show (MainAST (ASTInt n)) = show n
    show (MainAST (ASTBool b)) = if b then "#t" else "#f"
    show _ = ""
