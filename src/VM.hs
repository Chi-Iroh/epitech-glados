module VM where

import Data.Functor ((<&>))
import Data.Word (Word8, Word32)
import System.Exit (die)
import Text.Printf (printf)
import Unsafe.Coerce (unsafeCoerce)

import AssemblyInstructions (AssemblyInstruction(..))
import BinaryIO (readBinary, writeBinary)

import Bits (splitWord32, combineWord32)
import Deserialize (deserialize, deserializeType, deserializeInt)
import SymbolTable (readSymbolTable, SymbolTable)
import Serialize (Serializable, serialize)
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
deserializeList a len bytes list = deserialize a bytes >>= (\(member, len', rest) -> deserializeList a (len + len') rest $ list ++ [member])

deserializeTypeAndValue :: [Word8] -> Safe (Any, Int)
deserializeTypeAndValue bytes = deserializeType bytes >>= (\(_type, len, rest) -> deserialize _type rest >>= \(a, len', rest') -> Value (Any (_type, a), len + len'))

parseInstruction :: [Word8] -> Safe (AssemblyInstruction, Int)
parseInstruction (0x00 : xs) = mapFst PushValue <$> deserializeTypeAndValue xs
parseInstruction (0x01 : reg : xs) = Value (PushRegister reg, 2)
parseInstruction (0x10 : reg : xs) = Value (Pop reg, 2)
parseInstruction (0x20 : xs) = deserializeType xs >>=(\(_type, len, rest) -> deserializeInt xs >>= \(a, len', _) -> Value (Construct _type a, len + len'))
parseInstruction (0x30 : reg : xs) = Value (Test reg, 2)
parseInstruction (0x40 : byte1 : byte2 : byte3 : byte4 : xs) = Value (JumpIfTrue (combineWord32 $ byte1 : [byte2] ++ [byte3] ++ [byte4]), 5)
parseInstruction (0x50 : byte1 : byte2 : byte3 : byte4 : xs) = Value (JumpIfFalse (combineWord32 $ byte1 : [byte2] ++ [byte3] ++ [byte4]), 5)
parseInstruction (0x60 : xs) = deserializeList T_Char 0 xs [] <&> (\(str, i) -> (map (\(Any (_, a)) -> unsafeCoerce a :: Char) str, i)) <&> mapFst Call
parseInstruction (0x70 : xs) = mapFst RetValue <$> deserializeTypeAndValue xs
parseInstruction (0x71 : reg : xs) = Value (RetRegister reg, 2)
parseInstruction (0x80 : reg : xs) = mapFst (MovValue reg) <$> deserializeTypeAndValue xs
parseInstruction (0x81 : reg1 : reg2 : xs) = Value (MovRegister reg1 reg2, 3)
parseInstruction (0x90 : xs) = mapFst OutValue <$> deserializeTypeAndValue xs
parseInstruction (0x91 : reg : xs) = Value (RetRegister reg, 2)
parseInstruction (_ : xs) = parseInstruction xs
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

data Any = forall a. (Serializable a, Show a) => Any (Type, a)
type Address = Word32

instance Serializable Any where
    serialize (Any (_, a)) = serialize a

instance Show Any where
    show :: Any -> String
    show (Any (type', val)) = printf "Any (%s, %s)" (show type') (show val)

data VM = VM {
    _registers :: [Any],    -- 16 registers
    _callStack :: [Int],    -- call stack for function calls
    _bf :: Maybe Bool,      -- boolean flag for branching
    _valueStack :: [Any],   -- value stack (where args are pushed)
    _pc :: Address          -- position of current opcode
}

addrToBytes :: Address -> [Word8]
addrToBytes = splitWord32