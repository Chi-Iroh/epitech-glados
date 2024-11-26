module AST (AST(ASTDefine, ASTSymbol, ASTNumber, ASTBoolean, ASTCall)) where

data AST = ASTDefine String AST | ASTSymbol String | ASTNumber Int | ASTBoolean Bool | ASTCall String [AST] deriving Show
