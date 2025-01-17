module Converter (
    getParameter,
    toParam,
    toLambdaParamsList,
    sexprSListHandling,
    sexprToAST,
    convert
    ) where

import SExpression
import AST
import Utils
import Type
import qualified Data.Text as DText
import qualified Data.Text.Internal.Search as DTextIS
import qualified Data.List as DList
import qualified Data.Tuple as DTuple

-------------------------------------------------------------------------------

getTypeSExpr :: SExpr -> Type
getTypeSExpr (SSymbol "int") = T_Int
getTypeSExpr (SSymbol "uint") = T_UInt
getTypeSExpr (SSymbol "char") = T_Char
getTypeSExpr (SSymbol "float") = T_Float
getTypeSExpr (SSymbol "bool") = T_Bool
getTypeSExpr (SSymbol "string") = T_String
getTypeSExpr (SSymbol "procedure") = T_Procedure
getTypeSExpr (SSymbol "type") = T_Type
getTypeSExpr (SSymbol "integer") = typeInteger
getTypeSExpr (SSymbol "number") = typeNumber
getTypeSExpr (SSymbol "any") = typeAny
getTypeSExpr (SSymbol "a") = T_Template
getTypeSExpr (STuple (a:b:rest))
    | null rest = T_Tuple (getTypeSExpr a, getTypeSExpr b)
    | otherwise = T_Undefined
getTypeSExpr (STuple _) = T_Undefined
getTypeSExpr (SArray [a]) = T_List $ getTypeSExpr a
getTypeSExpr (SArray _) = T_Undefined
getTypeSExpr _ = T_Undefined

-------------------------------------------------------------------------------

paramName :: String -> String
paramName str = DTuple.fst $ splitAt (head $ DTextIS.indices (DText.pack "::") (DText.pack str)) str

paramType :: String -> String
paramType str = DTuple.snd $ splitAt ((+) 2 $ head $ DTextIS.indices (DText.pack "::") (DText.pack str)) str

checkParam :: String -> Type -> Safe Parameter
checkParam str t
    | length str < 1 = Error "Glados: SyntaxError: No symbol found in a parameter definition."
    | otherwise = Value (ASTProcedure $ str, t)

getParameter :: String -> Safe Parameter
getParameter str
    | len == 1 = checkParam (paramName str) (getTypeSExpr $ SSymbol $ paramType str)
    | otherwise = Error ("Glados: SyntaxError: " ++ str ++ " isn't a valid parameter.")
    where
        len = length $ DTextIS.indices (DText.pack "::") (DText.pack str)

-------------------------------------------------------------------------------

toParam :: SExpr -> Safe Parameter
toParam (SSymbol s) = getParameter s
toParam arg = Error ((show arg) ++ " isn't a valid lambda parameter, a SSymbol was expected !")

toLambdaParamsList :: SExpr -> Safe [Parameter]
toLambdaParamsList (SList params) = mapM toParam params
toLambdaParamsList params = Error (show params ++ " isn't a valid lambda parameters list, a SList was expected !")

-- convert a SList to an Maybe AST, supposed to handle some error (currently not implemented redefine)
sexprSListHandling :: [SExpr] -> Safe AST
sexprSListHandling [] = Error "GLaDOS: ConverterError: Expected a list of at least one SExpr but got an empty list instead.\n"
sexprSListHandling [SNumber a] = Value (ASTInt a)
sexprSListHandling [SSymbol "#t"] = Value (ASTBool True)
sexprSListHandling [SSymbol "#f"] = Value (ASTBool False)
sexprSListHandling [SSymbol a] = Value (ASTProcedure a)
sexprSListHandling [SArray elements] =
    case mapM (sexprSListHandling . pure) elements of
        Value astList -> Value (ASTArray astList)
        Error err -> Error err
sexprSListHandling (STuple(a:b):_) =
    case (sexprSListHandling [a], sexprSListHandling b) of
        (Value astA, Value astB) -> Value (ASTTuple (astA, astB))
        (Error err, _) -> Error err
        (_, Error err) -> Error err

sexprSListHandling (SList (SSymbol "lambda":parameters:body:[rType]):arguments) =
    case toLambdaParamsList parameters of
        Value parameter -> case sexprToAST [body] of
            Value [expression] -> case sexprToAST arguments of
                Value rest -> Value (ASTCall (LambdaCall parameter expression (getTypeSExpr rType)) rest)
                Error _ -> Value (ASTCall (LambdaCall parameter expression (getTypeSExpr rType)) []) -- if after lambdacall there is nothing
            Value _ -> Error "GLaDOS: ConverterError: The body of the lambda cannot be deducted. [Converter.hs]\n"
            Error err -> Error err
        Error err -> Error err

sexprSListHandling (SSymbol "lambda":parameters:body:[rType]) =
    case toLambdaParamsList parameters of
        Value parameter -> case sexprToAST [body] of
            Value [expression] -> Value (ASTLambda parameter expression (getTypeSExpr rType))
            Value _ -> Error "GLaDOS: ConverterError: Expected a one item list. [Converter.hs]\n"
            Error err -> Error err
        Error err -> Error err

sexprSListHandling (SSymbol "define":(SSymbol name):vType:[body]) =
    case sexprToAST [body] of
        Value [result] -> Value (ASTDefine name (getTypeSExpr vType) result)
        Value _ -> Error "GLaDOS: ConverterError: Expected a one item list. [Converter.hs]\n"
        Error err -> Error err

sexprSListHandling (SList elements:rest) =
    case sexprSListHandling elements of
        Value result -> case sexprSListHandling rest of
            Value restResult -> Value (ASTCall (LambdaCall [] result T_Undefined) [restResult]) -- Example handling
            Error err -> Error err
        Error err -> Error err

sexprSListHandling (SSymbol a:b) = case getSymbol (SSymbol a) of
        Value symbol -> case sexprToAST b of
            Value result -> Value (ASTCall (FunctionCall symbol) result)
            Error err -> Error err
        Error err -> Error err

sexprSListHandling _ = Error "GLaDOS: ConverterError: Not handled case. [Converter.hs]\n"

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
