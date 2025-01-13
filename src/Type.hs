{-# LANGUAGE InstanceSigs #-}

module Type (
    Type(..),
    verifyTypeTuple,
    verifyTypeList,
    combinateTypes
    ) where

import Data.List (intercalate)
import Utils

data Type = T_Int                   |
            T_UInt                  |
            T_Char                  |
            T_Float                 |
            T_Bool                  |
            T_Tuple (Type, Type)    |
            T_List Type             |
            T_EmptyList             |
            T_String                |
            T_Procedure             |
            T_Function [Type] Type  |
            T_Combination [Type]    |
            T_NULL                  |
            T_Undefined             deriving Eq

instance Show Type where
    show :: Type -> String
    show T_Int = "int"
    show T_UInt = "uint"
    show T_Char = "char"
    show T_Float = "float"
    show T_Bool = "bool"
    show (T_Tuple (left, right)) = "{" ++ show left ++ ", " ++ show right ++ "}"
    show (T_List a) = "[" ++ show a ++ "]"
    show T_EmptyList = "[]"
    show T_String = show (T_List T_Char)
    show T_Procedure = "<procedure>"
    show (T_Function params ret) = "{" ++ params' ++ show ret ++ "}"
        where params' = if null params then "" else (intercalate " " $ map (\param -> "(" ++ show param ++ ")") params) ++ " => "
    show (T_Combination types)
        | null types = "<empty combination type>"
        | otherwise = intercalate "|" (map show types)
    show T_NULL = "NULL"
    show T_Undefined = "<undefined>"

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
