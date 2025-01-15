module Vm where

import SymbolTable (readSymbolTable)
import Data.Word (Word8, Word32)
import Text.Printf (printf)
import Bits (splitWord32)
import Type (Type(..))
import BinaryIO (readBinary, writeBinary)
import System.Posix (fileAccess)
import Utils (Safe(..))
import VMData (Address, addrToBytes, VM, Any)
import Distribution.Simple (KnownExtension(Safe))

pushVal = 0x00
pushReg = 0x01
pop = 0x10
test = 0x20
jt = 0x30
jf = 0x40
call = 0x50
retVal = 0x60
retReg = 0x61
movVal = 0x70
movReg = 0x71
outVal = 0x80
outReg = 0x81

deserialize :: [Word8] -> Safe (Any, [Word8])
deserialize (a:as) = Value (2,as)

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

mainVM :: FilePath -> IO ()
mainVM path = do
    file <- readBinary path
    let (table, rest) = readSymbolTable file
    writeBinary "output.txt" $ parseByte rest

-- to do : 
-- - sortie de parseByte = fst -> associated call && snd -> parseByte snd
-- - init VM data
-- - pass it through 