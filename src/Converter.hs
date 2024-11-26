module Converter (sexprToAST) where

import SExpression
import AST

sexprToAST :: SExpr -> Maybe AST
sexprToAST (SNumber n) = Just $ ASTNumber n
sexprToAST (SSymbol "#t") = Just $ ASTBoolean True
sexprToAST (SSymbol "#f") = Just $ ASTBoolean False
sexprToAST (SSymbol "define") = Nothing
sexprToAST (SSymbol x) = Just $ ASTSymbol x
sexprToAST (SList []) = Nothing
sexprToAST (SList [(SSymbol "define")]) = Nothing
sexprToAST (SList [(SSymbol "define"), (SSymbol x), expr]) = sexprToAST expr >>= (\expr -> Just $ ASTDefine x expr)
sexprToAST (SList ((SSymbol x) : xs))
    | x == "define" = Nothing
    | otherwise = mapM sexprToAST xs >>= (\xs -> Just $ ASTCall x xs)
