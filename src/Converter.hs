module Converter (convert) where

import SExpression
import AST
import Utils

-- convert a SList to an Maybe AST, supposed to handle some error (currently not implemented redefine)
sexprSListHandling :: [SExpr] -> Safe AST
sexprSListHandling [] = Error "Error in processing the conversion"
sexprSListHandling [SNumber a] = Value (ASTNumber a)
sexprSListHandling [SSymbol "#t"] = Value (ASTBoolean True)
sexprSListHandling [SSymbol "#f"] = Value (ASTBoolean True)
sexprSListHandling [SSymbol a] = Value (ASTSymbol a)
sexprSListHandling (SList (SSymbol "lambda":b:c):rest)
        | null c = Error "Error in processing the conversion"
        | otherwise = case sexprToAST [b] of
            Value parameter -> case sexprToAST c of
                Value [expression] -> case sexprToAST rest of
                    Value rest -> Value (ASTCall (LambdaCall parameter expression) rest)
                    Error _ -> Value (ASTCall (LambdaCall parameter expression) [])             -- if after lambdacall there is nothing
                Error err -> Error err                                                          -- lambda must have expression
            Error _ -> case sexprToAST c of
                Value [expression] -> case sexprToAST rest of
                    Value rest -> Value (ASTCall (LambdaCall [] expression) rest)
                    Error _ -> Value (ASTCall (LambdaCall [] expression) [])                    -- if after lambdacall there is nothing
                Error err -> Error err                                                          -- lambda must have expression

sexprSListHandling (SSymbol "define":b:c)
        | null [b] = Error "Error in processing the conversion"                                 -- define must have a name
        | null c = Error "Error in processing the conversion"                                   -- defne must have a value
        | otherwise = case getSymbol b of
            Value symbol -> case sexprToAST c of
                Value [result] -> Value (ASTDefine symbol result)
                Error err -> Error err                                                          -- cannot define variable without value in scheme
            Error _ -> case sexprToAST c of                                                     -- if no symbol
                Value [result] -> Value (ASTDefine "" result)
                Error err -> Error err                                                          -- cannot define variable without value in scheme

sexprSListHandling (SSymbol "define":SList(a:b):c)
        | null [c] = Error "Error in processing the conversion"
        | otherwise = case getSymbol a of
            Value symbol -> case sexprToAST [SList (SSymbol "lambda" : SList b : c)] of
                Value [result] -> Value (ASTDefine symbol result)
                Error err -> Error err                                                          -- cannot define variable without value in scheme
            Error _ -> case sexprToAST c of                                                     -- if no symbol
                Value [result] -> Value (ASTDefine "" result)
                Error err -> Error err                                                          -- cannot define variable without value in scheme

sexprSListHandling (SSymbol "define":b:c)
        | null [b] = Error "Error in processing the conversion"                                 -- define must have a name
        | null c = Error "Error in processing the conversion"                                   -- defne must have a value
        | otherwise = case getSymbol b of
            Value symbol -> case sexprToAST c of
                Value [result] -> Value (ASTDefine symbol result)
                Error err -> Error err                                                          -- cannot define variable without value in scheme
            Error _ -> case sexprToAST c of                                                     -- if no symbol
                Value [result] -> Value (ASTDefine "" result)
                Error err -> Error err                                                          -- cannot define variable without value in scheme

sexprSListHandling (SSymbol "lambda":b:c)
        | null c = Error "Error in processing the conversion"                                   -- lambda must have som expression (at least 1) but can have no parameter
        | otherwise = case sexprToAST [b] of
            Value parameter -> case sexprToAST c of
                Value [expression] -> Value (ASTLambda parameter expression)
                Error err -> Error err                                                          -- lambda must have som expression (at least 1) but can have no parameter
            Error _ -> case sexprToAST c of
                Value [expression] -> Value (ASTLambda [] expression)
                Error err -> Error err                                                          -- lambda must have som expression (at least 1) but can have no parameter

sexprSListHandling (SSymbol a:b) = case getSymbol (SSymbol a) of
        Value symbol -> case sexprToAST b of
            Value result -> Value (ASTCall (FunctionCall symbol) result)

sexprSListHandling (a:b) = Error "Error in processing the conversion" -- case de merde que je veux pas

-- sexprSListHandling _ = Value (ASTNumber 5) --debug

sexprToAST :: [SExpr] -> Safe [AST]
sexprToAST [] = Value []
sexprToAST (SList elements : rest) =
    case sexprSListHandling elements of
        Value result ->
            case sexprToAST rest of
                Value restAST -> Value (result : restAST)
                Error err -> Error err                                                      -- Error in processing the rest
        Error err -> Error err                                                              -- Error in processing the current list
sexprToAST (a : rest) =
    case sexprSListHandling [a] of
        Value resultA ->
            case sexprToAST rest of
                Value resultRest -> Value (resultA : resultRest)
                Error err -> Error err
        Error err -> Error err

    -- sexprToAST _ = Value [ASTNumber 4] --debug

convert :: Safe [SExpr] -> Safe [AST]
convert (Error err) = Error err
convert (Value list) = sexprToAST list 

