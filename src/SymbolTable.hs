module SymbolTable (writeSymbolTable, readSymbolTable, symbolTableEnd, SymbolTable) where

import Data.ByteString.Internal (w2c)
import Data.List (isPrefixOf)
import Data.Word (Word8)
import Bits (splitWord32, combineWord32)
import Serialize (serializeChar)
import VM (Address)

type Symbol = (String, Address)
type SymbolTable = [Symbol]

symbolTableEnd :: [Word8]
symbolTableEnd = [0x11, 0x12, 0x13, 0x00]

encodeString :: String -> [Word8]
encodeString = (++ [0x00]) . concatMap serializeChar

writeSymbolTable :: SymbolTable -> [Word8]
writeSymbolTable = (++ symbolTableEnd) . concatMap writeSymbol
    where writeSymbol (sym, addr) = encodeString sym ++ splitWord32 addr

splitAtPattern :: Eq a => [a] -> [a] -> ([a], [a])
splitAtPattern [] list = (list, [])
splitAtPattern pattern list@(x : xs)
    | isPrefixOf pattern list = ([], drop (length pattern) list)
    | otherwise = (x : before, after)
    where (before, after) = splitAtPattern pattern xs

splitAtElem :: Eq a => a -> [a] -> ([a], [a])
splitAtElem _ [] = ([], [])
splitAtElem a (x : xs)
    | a == x = ([], xs)
    | otherwise = (x : beforeA, afterA)
    where (beforeA, afterA) = splitAtElem a xs

readSymbol :: [Word8] -> (Symbol, [Word8])
readSymbol bytes = ((map w2c name, combineWord32 addr), rest')
    where (name, rest) = splitAtElem 0x00 bytes
          (addr, rest') = splitAt 4 rest

readSymbolTable' :: [Word8] -> SymbolTable
readSymbolTable' bytes
    | null rest = [symbol]
    | otherwise = symbol : readSymbolTable' rest
    where (symbol, rest) = readSymbol bytes

readSymbolTable :: [Word8] -> (SymbolTable, [Word8])
readSymbolTable bytes
    | null table = ([], rest)
    | otherwise = (readSymbolTable' table, rest)
    where (table, rest) = splitAtPattern symbolTableEnd bytes