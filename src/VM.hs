module VM where

import Data.Word (Word8, Word32)
import Text.Printf (printf)
import System.Exit (die)

import BinaryIO (readBinary, writeBinary)
import Bits (splitWord32)
import ByteCode
import SymbolTable (readSymbolTable, SymbolTable)
import Type (Type(..))
import Utils (Safe(..))
import VMData (Address, addrToBytes, Vm(..), Any)
import Text.XHtml (table)

deserialize :: [Word8] -> Safe (Any, [Word8])
deserialize (a:as) = Error "e" -- Value (2,as) -- en commentaire car le 2 compile pas

fromSafe :: Safe a -> a
fromSafe (Error err) = error err
fromSafe (Value a) = a

pushRegister :: Int -> [Maybe Any] -> [Any] -> Safe [Any]
pushRegister 0 ((Just a):as) stack = Value $ a:stack
pushRegister 0 (Nothing:as) _ = Error "Register empty"
pushRegister x (a:as) stack = pushRegister (x - 1) as stack
pushRegister _ [] _ = Error "Register out of bounds"

pushValue :: Any -> [Any] -> Safe [Any]
pushValue a stack = Value $ a:stack

popStack :: Int -> [Any] -> [Maybe Any] -> Safe [Maybe Any]
popStack 0 (a:as) (_:rs) = Value $ Just a : rs
popStack x stack (r:rs) = popStack (x - 1) stack rs
popStack _ [] _ = Error "Stack empty"
popStack _ _ [] = Error "Register out of bounds"

parseByte :: Vm -> SymbolTable -> [Word8] -> [Word8]
parseByte (Vm r c bf v pc) table (pushVal : xs) = snd (fromSafe $ deserialize xs)
parseByte (Vm r c bf v pc) table (pushReg : xs) = snd (fromSafe $ deserialize xs)
parseByte (Vm r c bf v pc) table (pop : xs) = snd (fromSafe $ deserialize xs)
parseByte (Vm r c bf v pc) table (construct : xs) = snd (fromSafe $ deserialize xs)
parseByte (Vm r c bf v pc) table (test : xs) = snd (fromSafe $ deserialize xs)
parseByte (Vm r c bf v pc) table (jt : xs) = snd (fromSafe $ deserialize xs)
parseByte (Vm r c bf v pc) table (jf : xs) = snd (fromSafe $ deserialize xs)
parseByte (Vm r c bf v pc) table (call : xs) = snd (fromSafe $ deserialize xs)
parseByte (Vm r c bf v pc) table (retVal : xs) = snd (fromSafe $ deserialize xs)
parseByte (Vm r c bf v pc) table (retReg : xs) = snd (fromSafe $ deserialize xs)
parseByte (Vm r c bf v pc) table (movVal : xs) = snd (fromSafe $ deserialize xs)
parseByte (Vm r c bf v pc) table (movReg : xs) = snd (fromSafe $ deserialize xs)
parseByte (Vm r c bf v pc) table (outVal : xs) = snd (fromSafe $ deserialize xs)
parseByte (Vm r c bf v pc) table (outReg : xs) = snd (fromSafe $ deserialize xs)
parseByte _  _ [] = []

mainVM :: FilePath -> IO ()
mainVM path = do
    file <- readBinary path
    case readSymbolTable file of
        Error err -> die err
        Value (table, rest) -> writeBinary "output.txt" $ parseByte (Vm (replicate 16 Nothing) [] Nothing [] 0) table rest

-- to do : 
-- - sortie de parseByte = fst -> associated call && snd -> parseByte snd
-- - init VM data
-- - pass it through 
-- scope changing cond : call
-- deserialize xs >>= 