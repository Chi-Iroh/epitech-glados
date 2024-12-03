module AST
    (Call(LambdaCall, FunctionCall),
    AST(ASTDefine, ASTLambda, ASTSymbol, ASTNumber, ASTBoolean, ASTCall)
    ) where

data Call = LambdaCall [AST] AST | FunctionCall String deriving Show
data AST = ASTDefine String AST | ASTSymbol String | ASTNumber Int | ASTBoolean Bool | ASTCall Call [AST] | ASTLambda [AST] AST deriving Show