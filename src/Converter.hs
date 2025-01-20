module Converter (
    getParameter,
    toParam,
    toParamsList,
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

toParamsList :: SExpr -> Safe [Parameter]
toParamsList (SList params) = mapM toParam params
toParamsList params = Error (show params ++ " isn't a valid lambda parameters list, a SList was expected !")

toLambda :: [SExpr] -> Safe AST
toLambda (SSymbol "lambda":parameters:body:[rType]) =
    case toParamsList parameters of
        Value parameter -> case sexprToAST [body] of
            Value [expression] -> Value (ASTLambda parameter expression (getTypeSExpr rType))
            Value _ -> Error "GLaDOS: ConverterError: The lambda's body cannot be deducted. [Converter.hs]\n"
            Error err -> Error err
        Error err -> Error err
toLambda _ = Error "GLaDOS: ConverterError: Invalid lambda declaration.\n"

-------------------------------------------------------------------------------

lambdaToLambdaCall :: Safe AST -> Safe Call
lambdaToLambdaCall (Value (ASTLambda parameters body returnType)) = Value $ LambdaCall parameters body returnType
lambdaToLambdaCall (Error err) = Error err
lambdaToLambdaCall _ = Error "A non-lambda argument encountered in a lambda to lambdaCall cast."

lambdaToFunction :: Safe AST -> String -> Safe AST
lambdaToFunction _ "" = Error "A function name cannot be null."
lambdaToFunction (Value (ASTLambda parameters body returnType)) name = Value $ ASTFunction name parameters body returnType
lambdaToFunction (Error err) _ = Error err
lambdaToFunction _ _ = Error "A non-lambda argument encountered in a lambda to function cast."

makeSafeIf :: Safe [AST] -> Safe [AST] -> Safe [AST] -> Safe AST
makeSafeIf (Error err) _ _ = Error $ "An error encountered in the If's condition : " ++ err
makeSafeIf _ (Error err) _ = Error $ "An error encountered in the If's true statment : " ++ err
makeSafeIf _ _ (Error err) = Error $ "An error encountered in the If's false statment : " ++ err
makeSafeIf (Value [condition]) (Value [trueStatment]) (Value [falseStatment]) = Value $ ASTIf condition trueStatment falseStatment
makeSafeIf _ _ _ = Error "Badly formated If expression."

-------------------------------------------------------------------------------

-- convert a SList to an Maybe AST, supposed to handle some error (currently not implemented redefine)
sexprSListHandling :: [SExpr] -> Safe AST
sexprSListHandling [] = Error "GLaDOS: ConverterError: Expected a list of at least one SExpr but got an empty list instead.\n"

-- number
sexprSListHandling [SNumber a] = Value $ ASTInt a

--float
sexprSListHandling [SFloat a] = Value $ ASTFloat a

-- boolean
sexprSListHandling [SSymbol "#t"] = Value $ ASTBool True
sexprSListHandling [SSymbol "#f"] = Value $ ASTBool False

-- NULL
sexprSListHandling [SSymbol "NULL"] = Value ASTNULL

--String
sexprSListHandling [SString str] = Value $ ASTString str

-- procedure (variable or function name)
sexprSListHandling [SSymbol a] = Value (ASTProcedure a)

-- array
sexprSListHandling [SArray elements] =
    case mapM (sexprSListHandling . pure) elements of
        Value astList -> Value (ASTArray astList)
        Error err -> Error err

-- tuple
sexprSListHandling (STuple (a:b):_) =
    case (sexprSListHandling [a], sexprSListHandling b) of
        (Value astA, Value astB) -> Value (ASTTuple (astA, astB))
        (Error err, _) -> Error err
        (_, Error err) -> Error err

-- function declaration
sexprSListHandling (SSymbol "function":(SSymbol name):parameters:body:[rType]) = lambdaToFunction (toLambda (SSymbol "lambda":parameters:body:[rType])) name
sexprSListHandling (SSymbol "function":_) = Error "GLaDOS: ConverterError: Invalid function declaration. [Converter.hs]\n"

-- lambda call
sexprSListHandling (SList lambda@(SSymbol "lambda":_:_:[_]):arguments) =
    case lambdaToLambdaCall $ toLambda lambda of
        Value rLambda -> case sexprToAST arguments of
            Value rest -> Value (ASTCall (rLambda) rest)
            Error err -> Error err
        Error err -> Error err
sexprSListHandling (SList (SSymbol "lambda":_):_) = Error "GLaDOS: ConverterError: Invalid lambda call declaration. [Converter.hs]\n"

-- lambda declaration
sexprSListHandling lambda@(SSymbol "lambda":_:_:[_]) = toLambda lambda
sexprSListHandling (SSymbol "lambda":_) = Error "GLaDOS: ConverterError: Invalid lambda declaration. [Converter.hs]\n"

-- define declaration
sexprSListHandling (SSymbol "define":(SSymbol name):vType:[body]) =
    case sexprToAST [body] of
        Value [result] -> Value (ASTDefine name (getTypeSExpr vType) result)
        Value _ -> Error "GLaDOS: ConverterError: Expression not assignable in define declaration. [Converter.hs]\n"
        Error err -> Error err
sexprSListHandling (SSymbol "define":_) = Error "GLaDOS: ConverterError: Invalid define declaration. [Converter.hs]\n"

-- if declaration
sexprSListHandling (SSymbol "if":condition:trueStatment:[falseStatment]) = makeSafeIf (sexprToAST [condition]) (sexprToAST [trueStatment]) (sexprToAST [falseStatment])
sexprSListHandling (SSymbol "if":_) = Error "GLaDOS: ConverterError: Invalid if declaration. [Converter.hs]\n"

-- function call
sexprSListHandling (SSymbol symbol:rest) =
    case sexprToAST rest of
        Value arguments -> Value (ASTCall (FunctionCall symbol) arguments)
        Error err -> Error err

-- other
sexprSListHandling _ = Error "GLaDOS: ConverterError: Not handled case. [Converter.hs]\n"

-------------------------------------------------------------------------------

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
