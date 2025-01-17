module VM where

import Data.Word (Word8, Word32)
import Text.Printf (printf)
import System.Exit (die)

import AssemblyInstructions (AssemblyInstruction(..))
import BinaryIO (readBinary, writeBinary)
import Bits (splitWord32)
import Deserialize (deserialize, deserializeType)
import SymbolTable (readSymbolTable, SymbolTable)
import Type (Type(..))
import Utils (Safe(..))
import VMData (Address, addrToBytes, Vm(..), defaultVM, Any(..))

fromSafe :: Safe a -> a
fromSafe (Error err) = error err
fromSafe (Value a) = a

pushRegister :: Int -> [Maybe Any] -> [Any] -> Safe [Any]
pushRegister 0 ((Just a):_) stack = Value $ a:stack
pushRegister 0 (Nothing:_) _ = Error "Register empty"
pushRegister x (_:as) stack = pushRegister (x - 1) as stack
pushRegister _ [] _ = Error "Register out of bounds"

pushValue :: Any -> [Any] -> Safe [Any]
pushValue a stack = Value $ a:stack

popStack :: Int -> [Any] -> [Maybe Any] -> Safe [Maybe Any]
popStack 0 (a:_) (_:rs) = Value $ Just a : rs
popStack x stack (_:rs) = popStack (x - 1) stack rs
popStack _ [] _ = Error "Stack empty"
popStack _ _ [] = Error "Register out of bounds"

endOfFile :: String
endOfFile = "End of file"

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

deserializeList :: Type -> Int -> [Word8] -> [Any] -> Safe ([Any], Int)
deserializeList _ len (0x00:_) list = Value (list, len)
deserializeList _ _ [] _ = Error "No end bytes in list"
deserializeList a len bytes list = deserialize a bytes >>= (\(member, len', rest) -> deserializeList a (len + len') rest $ member : list)

deserializeTypeAndValue :: [Word8] -> Safe (Any, Int)
deserializeTypeAndValue bytes = deserializeType bytes >>= (\(_type, len, rest) -> deserialize _type rest >>= \(a, len', rest') -> Value (Any (_type, a), len + len'))

parseInstruction :: [Word8] -> Safe (AssemblyInstruction, Int)
parseInstruction (0x00 : xs) = mapFst PushValue <$> deserializeTypeAndValue xs
parseInstruction (0x01 : reg : xs) = Value (PushRegister reg, 2)
parseInstruction (0x10 : reg : xs) = Value (Pop reg, 2)
-- parseInstruction (0x20 : xs) = mapFst Construct <$> deserializeTypeAndValue xs >>= deserializeTypeAndValue xs
parseInstruction (0x30 : reg : xs) = Value (Test reg, 2)
-- parseInstruction (0x40 : xs) = snd (fromSafe $ deserialize xs)
-- parseInstruction (0x50 : xs) = snd (fromSafe $ deserialize xs)
-- parseInstruction (0x60 : xs) = mapFst Call <$> deserializeList T_Char 0 xs [] 
parseInstruction (0x70 : xs) = mapFst RetValue <$> deserializeTypeAndValue xs
parseInstruction (0x71 : reg : xs) = Value (RetRegister reg, 2)
parseInstruction (0x80 : reg : xs) = mapFst (MovValue reg) <$> deserializeTypeAndValue xs
parseInstruction (0x81 : reg1 : reg2 : xs) = Value (MovRegister reg1 reg2, 3)
parseInstruction (0x90 : xs) = mapFst OutValue <$> deserializeTypeAndValue xs
parseInstruction (0x91 : reg : xs) = Value (RetRegister reg, 2)
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