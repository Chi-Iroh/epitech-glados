{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Any (
    Any(..),
    makeAny,
    anyType
) where

import Data.ByteString.Internal (c2w)
import Data.Functor ((<&>))
import Data.Typeable (Typeable)
import Data.Word (Word8)

import Hex (showHex8)
import Serialize (Serializable(..), serializeUInt, serializeType, serializeTypeNull, serializeTypeEmptyList)
import Type (Type(..))
import Utils (Safe(..), safeCast)


data Any =  Int Int             |
            UInt Int            |
            Char Char           |
            Float Float         |
            Bool Bool           |
            EmptyArray          |
            Array [Any]         |
            Tuple (Any, Any)    |
            NULL                deriving (Eq, Typeable)

instance Show Any where
    show :: Any -> String
    show (Int n) = "int " ++ show n
    show (UInt n) = "uint " ++ show n
    show (Char c) = "char " ++ showHex8 (c2w c) ++ " (" ++ show c ++ ")"
    show (Float f) = "float " ++ show f
    show (Bool b) = "bool " ++ show b
    show EmptyArray = "[]"
    show (Array xs) = "[" ++ concatMap show xs ++ "]"
    show (Tuple (a, b)) = "{" ++ show a ++ ", " ++ show b ++ "}"
    show NULL = "NULL"

instance Serializable Any where
    serialize :: Any -> Safe [Word8]
    serialize (Int n) = serialize n
    serialize (UInt n) = serializeUInt n
    serialize (Char c) = serialize c
    serialize (Float f) = serialize f
    serialize (Bool b) = serialize b
    serialize EmptyArray = Value serializeTypeEmptyList
    serialize (Array xs) = serialize xs
    serialize (Tuple tuple) = serialize tuple
    serialize NULL = Value serializeTypeNull

reduceList :: Eq a => String -> String -> [a] -> Safe a
reduceList emptyListError _ [] = Error emptyListError
reduceList _ valuesNotEqualError list@(a : _) = if all (== a) list then Value a else Error valuesNotEqualError

anyType :: Any -> Safe Type
anyType (Int _) = Value T_Int
anyType (UInt _) = Value T_UInt
anyType (Char _) = Value T_Char
anyType (Float _) = Value T_Float
anyType (Bool _) = Value T_Bool
anyType EmptyArray = Value T_EmptyList
anyType (Array xs) = mapM anyType xs >>= reduceList "Array must have at least 1 value, use EmptyArray instead !" "Array are homogeneous !" <&> T_List
anyType (Tuple (a, b)) = liftA2 (\a' b' -> T_Tuple (a', b')) (anyType a) (anyType b)
anyType NULL = Value T_NULL

makeAny :: Typeable a => Type -> a -> Safe Any
makeAny T_Int a = Int <$> (safeCast a :: Safe Int)
makeAny T_UInt a = UInt <$> (safeCast a :: Safe Int)
makeAny T_Char a = Char <$> (safeCast a :: Safe Char)
makeAny T_Float a = Float <$> (safeCast a :: Safe Float)
makeAny T_Bool a = Bool <$> (safeCast a :: Safe Bool)
makeAny (T_List _type) a = Array <$> mapM (makeAny _type) a
makeAny (T_Tuple (t1, t2)) (a, b) = liftA2 (\a' b' -> Tuple (a', b')) (makeAny t1 a) (makeAny t2 b)
makeAny T_NULL _ = Value NULL
makeAny T_EmptyList _ = Value EmptyArray
makeAny _type _ = Error ("Type " ++ show _type ++ " cannot be converted to Any value !")