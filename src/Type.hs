module Type (
    Type(..),
    verifyTypeTuple,
    verifyTypeList,
    combinateTypes,
    verifyType
    ) where

import Utils

data Type = T_Int | T_UInt | T_Char | T_Float | T_Bool | T_Tuple (Type, Type) | T_List Type | T_EmptyList | T_String | T_Procedure | T_Function [Type] Type | T_Combination [Type] | T_NULL | T_Undefined | T_Type deriving (Eq, Show)

type Parameter = Type
type Argument = Type

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
    | otherwise = Error "Glados: SyntaxError: This expression do not return anything."

combinateTypes' :: [Safe Type] -> [Type] -> Safe Type
combinateTypes' [] list = Value $ T_Combination $ reverse list
combinateTypes' ((Error err):_) _ = Error err
combinateTypes' ((Value x):xs) list = combinateTypes' xs (x:list)

combinateTypes :: [Safe Type] -> Safe Type
combinateTypes a = combinateTypes' a []

verifyType :: Parameter -> Argument -> Safe Bool
verifyType T_EmptyList _ = Error "Glados: SyntaxError: An empty list isn't a valid parameter type."
verifyType T_NULL _ = Error "Glados: SyntaxError: NULL isn't a valid parameter type."
verifyType T_Undefined _ = Error "Glados: SyntaxError: The parameter type cannot be deducted."
verifyType (T_Combination []) _ = Error "Glados: SyntaxError: A type combination cannot be empty."
verifyType _ T_Undefined = Error "Glados: SyntaxError: The argument type cannot be deducted."
verifyType _ T_NULL = Value True
verifyType (T_List _) T_EmptyList = Value True
verifyType T_Procedure (T_Function _ _) = Value True
verifyType (T_Combination ps) (T_Combination as) = Value $ foldr (&&) True $ fmap (\a -> foldr (\x y -> (x == a) || y) False ps) as
verifyType (T_Combination ps) a = Value $ foldr (\x y -> (x == a) || y) False ps
verifyType p a
    | p == a = Value True
    | otherwise = Value False

--paramType :: String -> String
--paramType 

--getType :: String -> Safe Type
--getType "int" = Value T_Int
--getType "uint" = Value T_UInt
--getType "char" = Value T_Char
--getType "float" = Value T_Float
--getType "bool" = Value T_Bool
--getType "string" = Value T_String
--getType "type" = Value T_Type
--getType str
--    | ...
--    | otherwise = Error ("Glados: SyntaxError: " ++ str ++ " isn't a valid type.")
