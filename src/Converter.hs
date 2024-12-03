module Converter (sexprToAST) where

import SExpression
-- import AST

data Call = LambdaCall [AST] AST | FunctionCall String deriving Show
data AST = ASTDefine String AST | ASTSymbol String | ASTNumber Int | ASTBoolean Bool | ASTCall Call [AST] | ASTLambda [AST] AST deriving Show

-- convert a SList to an Maybe AST, supposed to handle some error (currently not implemented redefine)
sexprSListHandling :: [SExpr] -> Maybe AST
sexprSListHandling [] = Nothing
sexprSListHandling [SNumber a] = Just (ASTNumber a)
sexprSListHandling [SSymbol "#t"] = Just (ASTBoolean True)
sexprSListHandling [SSymbol "#f"] = Just (ASTBoolean True)
sexprSListHandling [SSymbol a] = Just (ASTSymbol a)
sexprSListHandling (SList (SSymbol "lambda":b:c):rest)
        | null c = Nothing
        | otherwise = case sexprToAST [b] of
            Just parameter -> case sexprToAST c of
                Just [expression] -> case sexprToAST rest of
                    Just rest -> Just (ASTCall (LambdaCall parameter expression) rest)
                    Nothing -> Just (ASTCall (LambdaCall parameter expression) [])          -- if after lambdacall there is nothing
                Nothing -> Nothing                                                          -- lambda must have expression
            Nothing -> case sexprToAST c of
                Just [expression] -> case sexprToAST rest of
                    Just rest -> Just (ASTCall (LambdaCall [] expression) rest)
                    Nothing -> Just (ASTCall (LambdaCall [] expression) [])                 -- if after lambdacall there is nothing
                Nothing -> Nothing                                                          -- lambda must have expression

sexprSListHandling (SSymbol "define":b:c)
        | null [b] = Nothing                                                                -- define must have a name
        | null c = Nothing                                                                  -- defne must have a value
        | otherwise = case getSymbol b of
            Just symbol -> case sexprToAST c of
                Just [result] -> Just (ASTDefine symbol result)
                Nothing -> Nothing                                                          -- cannot define variable without value in scheme
            Nothing -> case sexprToAST c of                                                 -- if no symbol
                Just [result] -> Just (ASTDefine "" result)
                Nothing -> Nothing                                                          -- cannot define variable without value in scheme

sexprSListHandling (SSymbol "lambda":b:c)
        | null c = Nothing                                                                  -- lambda must have som expression (at least 1) but can have no parameter
        | otherwise = case sexprToAST [b] of
            Just parameter -> case sexprToAST c of
                Just [expression] -> Just (ASTLambda parameter expression)
                Nothing -> Nothing                                                          -- lambda must have som expression (at least 1) but can have no parameter
            Nothing -> case sexprToAST c of
                Just [expression] -> Just (ASTLambda [] expression)
                Nothing -> Nothing                                                          -- lambda must have som expression (at least 1) but can have no parameter

sexprSListHandling (SSymbol a:b) = case getSymbol (SSymbol a) of
        Just symbol -> case sexprToAST b of
            Just result -> Just (ASTCall (FunctionCall symbol) result)

sexprSListHandling (a:b) = Nothing -- case de merde que je veux pas

-- sexprSListHandling _ = Just (ASTNumber 5) --debug


sexprToAST :: [SExpr] -> Maybe [AST]
sexprToAST [] = Just []
sexprToAST (SList elements:rest) = case sexprSListHandling elements of
    Just result -> case sexprToAST rest of
        Just restAST -> Just (result : restAST)
        Nothing -> Nothing                                                                  -- Error in processing the rest
    Nothing ->  Nothing                                                                     -- Error in processing the current list
sexprToAST (a:rest) = case sexprSListHandling [a] of
    Just resultA -> case sexprToAST rest of
        Just resultRest -> Just (resultA : resultRest)
        Nothing -> Nothing
    Nothing -> Nothing

    -- sexprToAST _ = Just [ASTNumber 4] --debug



