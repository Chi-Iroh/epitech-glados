module Converter (
    converterListError,
    toParam,
    toLambdaParamsList,
    sexprSListHandling,
    sexprToAST,
    convert
    ) where

import SExpression
import AST
import Utils

converterListError :: String -> Int -> Safe AST
converterListError qualifier line = Error ("GLaDOS: ConverterError: Expected a one item list but got " ++ qualifier ++ " list instead. [Converter.hs:" ++ (show line) ++ "]\n")

toParam :: SExpr -> Safe AST
toParam (SSymbol s) = Value (ASTSymbol s)
toParam arg = Error ((show arg) ++ " isn't a valid lambda parameter, a SSymbol was expected !")

toLambdaParamsList :: SExpr -> Safe [AST]
toLambdaParamsList (SList params) = mapM toParam params
toLambdaParamsList params = Error (show params ++ " isn't a valid lambda parameters list, a SSymbol was expected !")

-- convert a SList to an Maybe AST, supposed to handle some error (currently not implemented redefine)
sexprSListHandling :: [SExpr] -> Safe AST
sexprSListHandling [] = Error "GLaDOS: ConverterError: Expected a list of at least one SExpr but got an empty list instead. [Converter.hs:12]\n"
sexprSListHandling [SNumber a] = Value (ASTNumber a)
sexprSListHandling [SSymbol "#t"] = Value (ASTBoolean True)
sexprSListHandling [SSymbol "#f"] = Value (ASTBoolean False)
sexprSListHandling [SSymbol a] = Value (ASTSymbol a)
sexprSListHandling (SList (SSymbol "lambda":b:c):rests)
        | null c = Error "GLaDOS: SyntaxError: Not enough arguments to declare a lambda.\n"
        | otherwise = case toLambdaParamsList b of
            Value parameter -> case sexprToAST c of
                Value [expression] -> case sexprToAST rests of
                    Value rest -> Value (ASTCall (LambdaCall parameter expression) rest)
                    Error _ -> Value (ASTCall (LambdaCall parameter expression) [])             -- if after lambdacall there is nothing
                Value [] -> converterListError "an empty" 32
                Value (_:_:_) -> converterListError "a bigger" 33
                Error err -> Error err
            Error _ -> case sexprToAST c of
                Value [expression] -> case sexprToAST rests of
                    Value rest -> Value (ASTCall (LambdaCall [] expression) rest)
                    Error _ -> Value (ASTCall (LambdaCall [] expression) [])                    -- if after lambdacall there is nothing
                Value [] -> converterListError "an empty" 39
                Value (_:_:_) -> converterListError "a bigger" 40
                Error err -> Error err

sexprSListHandling (SSymbol "lambda":b:c)
        | null c = Error "GLaDOS: SyntaxError: Not enough arguments to declare a lambda.\n"
        | otherwise = case toLambdaParamsList b of
            Value parameter -> case sexprToAST c of
                Value [expression] -> Value (ASTLambda parameter expression)
                Value [] -> converterListError "an empty" 48
                Value (_:_:_) -> converterListError "a bigger" 49
                Error err -> Error err
            Error _ -> case sexprToAST c of
                Value [expression] -> Value (ASTLambda [] expression)
                Value [] -> converterListError "an empty" 52
                Value (_:_:_) -> converterListError "a bigger" 54
                Error err -> Error err

sexprSListHandling (SSymbol "define":SList(a:b):c)
        | null [c] = Error "GLaDOS: SyntaxError: Define expression must assign something to the defined symbol.\n"
        | otherwise = case getSymbol a of
            Value symbol -> case sexprToAST [SList (SSymbol "lambda" : SList b : c)] of
                Value [result] -> Value (ASTDefine symbol result)
                Value [] -> converterListError "an empty" 62
                Value (_:_:_) -> converterListError "a bigger" 63
                Error err -> Error err
            Error _ -> case sexprToAST c of                                                     -- if no symbol
                Value [result] -> Value (ASTDefine "" result)
                Value [] -> converterListError "an empty" 67
                Value (_:_:_) -> converterListError "a bigger" 68
                Error err -> Error err

sexprSListHandling (SSymbol "define":b:c)
        | null [b] = Error "GLaDOS: SyntaxError: Define expression is missing the defined symbol.\n"
        | null c = Error "GLaDOS: SyntaxError: Define expression must assign something to the defined symbol.\n"
        | otherwise = case getSymbol b of
            Value symbol -> case sexprToAST c of
                Value [result] -> Value (ASTDefine symbol result)
                Value [] -> converterListError "an empty" 77
                Value (_:_:_) -> converterListError "a bigger" 78
                Error err -> Error err
            Error _ -> case sexprToAST c of                                                     -- if no symbol
                Value [result] -> Value (ASTDefine "" result)
                Value [] -> converterListError "an empty" 82
                Value (_:_:_) -> converterListError "a bigger" 83
                Error err -> Error err

sexprSListHandling (SSymbol a:b) = case getSymbol (SSymbol a) of
        Value symbol -> case sexprToAST b of
            Value result -> Value (ASTCall (FunctionCall symbol) result)
            Error err -> Error err
        Error err -> Error err

sexprSListHandling (_:_) = Error "GLaDOS: ConverterError: Not handled case. [Converter.hs:84]\n"

sexprToAST :: [SExpr] -> Safe [AST]
sexprToAST [] = Value []
sexprToAST (SList elements : rest) =
    case sexprSListHandling elements of
        Value result ->
            case sexprToAST rest of
                Value restAST -> Value (result : restAST)
                Error err -> Error err
        Error err -> Error err
sexprToAST (a : rest) =
    case sexprSListHandling [a] of
        Value resultA ->
            case sexprToAST rest of
                Value resultRest -> Value (resultA : resultRest)
                Error err -> Error err
        Error err -> Error err

convert :: Safe [SExpr] -> Safe [AST]
convert (Error err) = Error err
convert (Value list) = sexprToAST list 
