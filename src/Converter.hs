module Converter (sexprToAST) where

import SExpression
import AST

-- data SExpr = SNumber Int | SSymbol String | SList [SExpr] deriving Show
-- data AST = ASTDefine String AST | ASTLambda [AST] AST | ASTSymbol String | ASTNumber Int | ASTBoolean Bool | ASTCall String [AST] deriving Show

-- convert a SList to an Maybe AST, supposed to handle some error (currently not implemented redefine)
sexprSListHandling :: [SExpr] -> Maybe AST
sexprSListHandling [] = Nothing
sexprSListHandling [SNumber a] = Just (ASTNumber a)
sexprSListHandling [SSymbol "#t"] = Just (ASTBoolean True)
sexprSListHandling [SSymbol "#f"] = Just (ASTBoolean True)
sexprSListHandling [SSymbol a] = Just (ASTSymbol a)
sexprSListHandling (SSymbol "define":b:c)
        | null [b] = Nothing    -- define must have a name
        | null c = Nothing      -- defne must have a value
        | otherwise = case getSymbol b of
            Just symbol -> case sexprSListHandling c of
                Just result -> Just (ASTDefine symbol result)
                Nothing -> Nothing  -- cannot define variable without value in scheme
            Nothing -> Nothing      -- if symbol is not a valid Ssymbol (should not happend)

sexprSListHandling (SSymbol "lambda":b:c)
        | null c = Nothing  -- lambda must have som expression (at least 1) but can have no parameter -- handle this case beacause currently not
        | otherwise = case sexprToAST [b] of
            Just parameter -> case sexprSListHandling c of
                Just expression -> Just (ASTLambda parameter expression)
                Nothing -> Nothing
            Nothing -> Nothing

sexprSListHandling _ = Just (ASTNumber 5) --debug



sexprToAST :: [SExpr] -> Maybe [AST]
sexprToAst [a] = sexprSListHandling [a]
sexprToAST (SList []:b) = Nothing
sexprToAST (SList (a:b):rest) = case sexprSListHandling (a:b) of
        Just result -> case sexprToAST b of
            Just rest -> Just (result : rest)
            Nothing -> Nothing  -- if error in the rest
        Nothing -> Nothing      -- if error in the result

sexprToAST _ = Just [ASTNumber 4] --debug



-- sexprToAST :: SExpr -> Maybe AST
-- sexprToAST (SNumber n) = Just $ ASTNumber n
-- sexprToAST (SSymbol "#t") = Just $ ASTBoolean True
-- sexprToAST (SSymbol "#f") = Just $ ASTBoolean False
-- sexprToAST (SSymbol "define") = Nothing
-- sexprToAST (SSymbol x) = Just $ ASTSymbol x
-- sexprToAST (SList []) = Nothing
-- sexprToAST (SList [(SSymbol "define")]) = Nothing
-- sexprToAST (SList [(SSymbol "define"), (SSymbol x), expr]) = sexprToAST expr >>= (\expr -> Just $ ASTDefine x expr)
-- sexprToAST (SList ((SSymbol x) : xs))
--     | x == "define" = Nothing
--     | otherwise = mapM sexprToAST xs >>= (\xs -> Just $ ASTCall x xs)
