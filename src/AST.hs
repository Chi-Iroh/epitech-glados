module AST (Call(..), AST(..), MainAST(..), isProcedureType) where

import Type
import Utils

type Parameter = (AST, Type)

data Call = LambdaCall [Parameter] AST Type | FunctionCall String deriving (Eq, Show)

-- ASTBoolean devient ASTBool ; ASTSymbol devient ASTProcedure
data AST = ASTInt Int | ASTUInt Int | ASTChar Char | ASTFloat Float | ASTBool Bool | ASTTuple (AST, AST) | ASTArray [AST] | ASTList [AST] | ASTString String | ASTProcedure String | ASTDefine String Type AST | ASTFunction String [Parameter] AST Type | ASTLambda [Parameter] AST Type | ASTCall Call [AST] | ASTIf AST AST AST | AST | ASTNULL deriving (Eq, Show)

data MainAST = MainAST AST

getReturnType :: Safe Type -> Safe Type
getReturnType (Error err) = Error err
getReturnType (Value (T_Function _ t)) = Value t
getReturnType _ = Error "Glados: TypeError: Mixed types within a list."

getType :: AST -> Safe Type
getType (ASTInt _) = Value T_Int
getType (ASTUInt _) = Value T_UInt
getType (ASTChar _) = Value T_Char
getType (ASTFloat _) = Value T_Float
getType (ASTBool _) = Value T_Bool
getType (ASTTuple (a, b)) = verifyTypeTuple ((getType a), (getType b))
getType (ASTList []) = Value T_EmptyList
getType (ASTList [x]) = verifyTypeList [getType x]
getType (ASTList list) = verifyTypeList $ map getType list
getType (ASTString _) = Value T_String
getType (ASTProcedure _) = Value T_Procedure
getType (ASTDefine _ _ _) = Value T_Procedure
getType (ASTFunction _ parameters _ r) = Value T_Procedure
getType (ASTLambda parameters _ r) = Value $ T_Function (map snd parameters) r
getType (ASTCall (LambdaCall a b c) _) = getReturnType $ getType $ ASTLambda a b c
getType (ASTCall (FunctionCall _) _) = Value T_Undefined
getType (ASTIf _ a b) = combinateTypes ((getType a):(getType b):[])
getType (ASTArray []) = Value T_EmptyList
getType (ASTArray [x]) = verifyTypeList [getType x]
getType (ASTArray list) = verifyTypeList $ map getType list
getType ASTNULL = Value T_NULL

isProcedureType :: MainAST -> Bool
isProcedureType (MainAST (ASTLambda _ _ _)) = True
isProcedureType (MainAST (ASTDefine _ _ _)) = True
isProcedureType _ = False

instance Show MainAST where
    show (MainAST (ASTInt n)) = show n
    show (MainAST (ASTBool b)) = if b then "#t" else "#f"
    show _ = ""
