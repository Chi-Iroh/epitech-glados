module BinaryOperator(binaryBuiltins) where

import Data.Bits
import Data.Char (ord, chr)

import Any (Any(..))
import DataBuiltins (Symbols, BuiltinsSymbol(BackendBuiltins))
import Utils (Safe(..))

binaryBuiltins :: Symbols
binaryBuiltins = [ BackendBuiltins ("&", pdpBinaryOp "&" (.&.))
            ,   BackendBuiltins ("band", pdpBinaryOp "band" (.&.))
            ,   BackendBuiltins ("|", pdpBinaryOp "|" (.|.))
            ,   BackendBuiltins ("bor", pdpBinaryOp "bor" (.|.))
            ,   BackendBuiltins ("~", pdpBNot)
            ,   BackendBuiltins ("bnot", pdpBNot)
            ,   BackendBuiltins ("^", pdpBinaryOp "^" xor)
            ,   BackendBuiltins ("bxor", pdpBinaryOp "bxor" xor)
            ,   BackendBuiltins (">>", pdpBinaryOp ">>" (.>>.))
            ,   BackendBuiltins ("rshift", pdpBinaryOp ">>" (.>>.))
            ,   BackendBuiltins ("<<", pdpBinaryOp "<<" (.<<.))
            ,   BackendBuiltins ("lshift", pdpBinaryOp "<<" (.<<.))]


pdpBinaryOp :: String -> (Int -> Int -> Int) -> [Any] -> Safe Any
pdpBinaryOp _ f [Int a, Int b] = Value $ Int $ f a b
pdpBinaryOp _ f [Int a, UInt b] = Value $ Int $ f a b
pdpBinaryOp _ f [Int a, Char b] = Value $ Int $ f a (ord b)
pdpBinaryOp _ f [UInt a, UInt b] = Value $ UInt $ f a b
pdpBinaryOp _ f [Char a, UInt b] = Value $ UInt $ f (ord a) b
pdpBinaryOp _ f [Char a, Char b] = Value $ Char $ chr $ f (ord a) (ord b)
pdpBinaryOp name _ args = Error ("Bad arguments when attempting to call " ++ name ++ " ! Expected 2 integers but got " ++ show args ++ " !")

pdpBNot :: [Any] -> Safe Any
pdpBNot [Int a] = Value $ Int $ complement a
pdpBNot [UInt a] = Value $ UInt $ complement a
pdpBNot [Char a] = Value $ Char $ chr $ complement (ord a)
pdpBNot args = Error ("Bad arguments when attempting to call '~' ! Expected an integer but got " ++ show args ++ " !")
