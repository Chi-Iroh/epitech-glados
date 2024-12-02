module AST
    (Call(LambdaCall, FunctionCall),
    AST(ASTDefine, ASTLambda, ASTSymbol, ASTNumber, ASTBoolean, ASTCall)
    ) where

data Call = LambdaCall [AST] AST | FunctionCall String deriving (Eq, Show)
data AST = ASTDefine String AST | ASTSymbol String | ASTNumber Int | ASTBoolean Bool | ASTCall Call [AST] | ASTLambda [AST] AST deriving Eq

instance Show AST where
    show (ASTNumber n) = show n
    show (ASTBoolean b) = if b then "#t" else "#f"
    show (ASTLambda _ _) = "#\\<procedure\\>"
    show _ = ""
