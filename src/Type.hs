module Type (
    Type(..),
    typeInteger,
    typeNumber,
    typeAny,
    verifyTypeTuple,
    verifyTypeList,
    combinateTypes,
    verifyType
    ) where

import Utils
import Data.List (intercalate)

data Type = T_Int | T_UInt | T_Char | T_Float | T_Bool | T_Tuple (Type, Type) | T_List Type | T_EmptyList | T_String | T_Procedure | T_Function [Type] Type | T_Combination [Type] | T_NULL | T_Template | T_Type | T_Undefined

instance Eq Type where
    (==) T_String (T_List T_Char) = True
    (==) (T_List T_Char) T_String = True
    (==) T_Int T_Int = True
    (==) T_UInt T_UInt = True
    (==) T_Char T_Char = True
    (==) T_Float T_Float = True
    (==) T_Bool T_Bool = True
    (==) (T_Tuple (a, b)) (T_Tuple (a', b')) = (a == a') && (b == b')
    (==) (T_List a) (T_List a') = a == a'
    (==) T_EmptyList T_EmptyList = True
    (==) T_String T_String = True
    (==) T_Procedure T_Procedure = True
    (==) (T_Function a b) (T_Function a' b') = (a == a') && (b == b')
    (==) (T_Combination a) (T_Combination a') = a == a'
    (==) T_NULL T_NULL = True
    (==) T_Template T_Template = True
    (==) T_Type T_Type = True
    (==) T_Undefined T_Undefined = True
    (==) _ _ = False

instance Show Type where
    show T_Int = "int"
    show T_UInt = "uint"
    show T_Char = "char"
    show T_Float = "float"
    show T_Bool = "bool"
    show T_EmptyList = "[]"
    show T_String = "string"
    show T_Procedure = "procedure"
    show T_NULL = "NULL"
    show T_Template = "a"
    show T_Type = "type"
    show T_Undefined = "undefined"
    show (T_Tuple (a, b)) = "{" ++ (show a) ++ ", " ++ (show b) ++ "}"
    show (T_List a) = "[" ++ (show a) ++ "]"
    show (T_Function a b) = "<(" ++ (unwords $ map show a) ++ ") => " ++ (show b) ++ ">"
    show (T_Combination a) = intercalate "|" $ map show a

type Parameter = Type
type Argument = Type

-------------------------------------------------------------------------------

typeInteger :: Type
typeInteger = T_Combination [T_Int, T_UInt, T_Char]

typeNumber :: Type
typeNumber = T_Combination [typeInteger, T_Float]

typeAny :: Type
typeAny = T_Combination [typeNumber, T_Bool, T_Tuple (T_Template, T_Template), T_List T_Template, T_Procedure]

-------------------------------------------------------------------------------

verifyTypeTuple :: (Safe Type, Safe Type) -> Safe Type
verifyTypeTuple ((Error err1), (Error err2)) = Error ("2 Errors encountered at the same time: " ++ err1 ++ " ; " ++ err2)
verifyTypeTuple ((Error err), _) = Error err
verifyTypeTuple (_, (Error err)) = Error err
verifyTypeTuple ((Value a), (Value b)) = Value $ T_Tuple (a, b)

verifyTypeList :: [Safe Type] -> Safe Type
verifyTypeList [] = Value T_EmptyList
verifyTypeList [(Error err)] = Error err
verifyTypeList [(Value x)] = Value $ T_List x
verifyTypeList ((Error err):_) = Error err
verifyTypeList (x@(Value t):xs)
    | all (== x) xs = Value $ T_List t
    | otherwise = Error "Glados: TypeError: This expression do not return anything."

-------------------------------------------------------------------------------

combinateTypes' :: [Safe Type] -> [Type] -> Safe Type
combinateTypes' [] list = Value $ T_Combination $ reverse list
combinateTypes' ((Error err):_) _ = Error err
combinateTypes' ((Value x):xs) list = combinateTypes' xs (x:list)

-- take a list of Safe Type and return a Safe T_Combination of those Types
combinateTypes :: [Safe Type] -> Safe Type
combinateTypes a = combinateTypes' a []

-------------------------------------------------------------------------------

verifyType :: Parameter -> Argument -> Bool
verifyType T_EmptyList _ = False                                               -- invalid parameter type
verifyType T_NULL _ = False                                                    -- invalid parameter type
verifyType T_Undefined _ = False                                               -- invalid parameter type
verifyType (T_Combination []) _ = False                                        -- invalid parameter type
verifyType _ T_Undefined = False                                               -- invalid argument type
verifyType _ T_NULL = True
verifyType (T_List _) T_EmptyList = True
verifyType T_Procedure (T_Function _ _) = True
verifyType (T_Combination ps) (T_Combination as) = foldr (&&) True $ map (\a -> foldr (\x y -> (x == a) || y) False ps) as
verifyType (T_Combination ps) a = foldr (\x y -> (x == a) || y) False ps
verifyType param arg
    | param == arg = True
    | otherwise = False
