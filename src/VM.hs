module VM where

import Data.Word (Word8, Word32)
import Text.Printf (printf)
import System.Exit (die)

import BinaryIO (readBinary, writeBinary)
import Bits (splitWord32)
import ByteCode
import SymbolTable (readSymbolTable)
import Type (Type(..))
import Utils (Safe(..))
import VMData (Address, addrToBytes, VM, Any)

deserialize :: [Word8] -> Safe (Any, [Word8])
deserialize (a:as) = Error "e" -- Value (2,as) -- en commentaire car le 2 compile pas

fromSafe :: Safe a -> a
fromSafe (Error err) = error err
fromSafe (Value a) = a

parseByte :: [Word8] -> [Word8]
parseByte (pushVal : xs) = snd (fromSafe $ deserialize xs)
parseByte (pushReg : xs) = snd (fromSafe $ deserialize xs)
parseByte (pop : xs) = snd (fromSafe $ deserialize xs)
parseByte (test : xs) = snd (fromSafe $ deserialize xs)
parseByte (jt : xs) = snd (fromSafe $ deserialize xs)
parseByte (jf : xs) = snd (fromSafe $ deserialize xs)
parseByte (call : xs) = snd (fromSafe $ deserialize xs)
parseByte (retVal : xs) = snd (fromSafe $ deserialize xs)
parseByte (retReg : xs) = snd (fromSafe $ deserialize xs)
parseByte (movVal : xs) = snd (fromSafe $ deserialize xs)
parseByte (movReg : xs) = snd (fromSafe $ deserialize xs)
parseByte (outVal : xs) = snd (fromSafe $ deserialize xs)
parseByte (outReg : xs) = snd (fromSafe $ deserialize xs)
parseByte [] = []

mainVM :: FilePath -> IO ()
mainVM path = do
    file <- readBinary path
    case readSymbolTable file of
        Error err -> die err
        Value (table, rest) -> writeBinary "output.txt" $ parseByte rest

-- to do : 
-- - sortie de parseByte = fst -> associated call && snd -> parseByte snd
-- - init VM data
-- - pass it through 