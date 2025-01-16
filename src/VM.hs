module VM where

import Data.Word (Word8, Word32)
import Text.Printf (printf)
import System.Exit (die)

import AssemblyInstructions (AssemblyInstruction(..))
import BinaryIO (readBinary, writeBinary)
import Bits (splitWord32)
import ByteCode
import Deserialize (deserialize, deserializeType)
import SymbolTable (readSymbolTable, SymbolTable)
import Type (Type(..))
import Utils (Safe(..))
import VMData (Address, addrToBytes, Vm(..), defaultVM, Any(..))

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

endOfFile :: String
endOfFile = "End of file"

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

deserializeTypeAndValue :: [Word8] -> Safe (Any, Int)
deserializeTypeAndValue bytes = deserializeType bytes >>= (\(_type, len, rest) -> deserialize _type rest >>= \(a, len', rest') -> Value (Any (_type, a), len + len'))

parseInstruction :: [Word8] -> Safe (AssemblyInstruction, Int)
parseInstruction (pushVal : xs) = mapFst PushValue <$> deserializeTypeAndValue xs

-- parseInstruction (pushReg : reg : xs) = Safe (pushRegister reg)
-- parseInstruction (pop : xs) = snd (fromSafe $ deserialize xs)
-- parseInstruction (construct : xs) = snd (fromSafe $ deserialize xs)
-- parseInstruction (test : xs) = snd (fromSafe $ deserialize xs)
-- parseInstruction (jt : xs) = snd (fromSafe $ deserialize xs)
-- parseInstruction (jf : xs) = snd (fromSafe $ deserialize xs)
-- parseInstruction (call : xs) = snd (fromSafe $ deserialize xs)
-- parseInstruction (retVal : xs) = snd (fromSafe $ deserialize xs)
-- parseInstruction (retReg : xs) = snd (fromSafe $ deserialize xs)
-- parseInstruction (movVal : xs) = snd (fromSafe $ deserialize xs)
-- parseInstruction (movReg : xs) = snd (fromSafe $ deserialize xs)
-- parseInstruction (outVal : xs) = snd (fromSafe $ deserialize xs)
-- parseInstruction (outReg : xs) = snd (fromSafe $ deserialize xs)
parseInstruction [] = Error endOfFile -- maybe special handling to return 0 instead of 1 when  end of file, because it's totally normal behavior

mainVM :: FilePath -> IO ()
mainVM path = do
    file <- readBinary path
    case readSymbolTable file of
        Error err -> die err
        Value (table, rest) -> case parseInstruction rest of
            Error err' -> die err'
            Value v -> print v
            -- writeBinary "output.txt"

-- to do : 
-- - sortie de parseByte = fst -> associated call && snd -> parseByte snd
-- - init VM data
-- - pass it through 
-- scope changing cond : call
-- deserialize xs >>= 