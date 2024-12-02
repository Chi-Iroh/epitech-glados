module AST (AST(ASTDefine, ASTLambda, ASTSymbol, ASTNumber, ASTBoolean, ASTCall)) where

data AST = ASTDefine String AST | ASTLambda [AST] AST | ASTSymbol String | ASTNumber Int | ASTBoolean Bool | ASTCall String [AST] deriving (Eq, Show)
