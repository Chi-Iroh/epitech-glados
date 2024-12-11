module AST (Call(..), AST(..), MainAST(..), isProcedureType) where

data Call = LambdaCall [AST] AST | FunctionCall String deriving (Eq, Show)

--
data AST = ASTDefine String AST | ASTSymbol String | ASTNumber Int | ASTBoolean Bool | ASTCall Call [AST] | ASTLambda [AST] AST deriving (Eq, Show)
data MainAST = MainAST AST

isProcedureType :: MainAST -> Bool
isProcedureType (MainAST (ASTLambda _ _)) = True
isProcedureType (MainAST (ASTDefine _ _)) = True
isProcedureType _ = False

instance Show MainAST where
    show (MainAST (ASTNumber n)) = show n
    show (MainAST (ASTBoolean b)) = if b then "#t" else "#f"
    show _ = ""
