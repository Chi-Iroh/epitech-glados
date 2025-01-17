module VM where

import Data.Functor ((<&>))
import Data.Word (Word8, Word32)
import System.Exit (die)
import Text.Printf (printf)
import Unsafe.Coerce (unsafeCoerce)

import AssemblyInstructions (AssemblyInstruction(..), RegisterID)
import BinaryIO (readBinary, writeBinary)
import Bits (splitWord32, combineWord32, u32)
import Deserialize (deserialize, deserializeType, deserializeInt)
import SymbolTable (readSymbolTable, SymbolTable)
import Serialize (Serializable, serialize)
import Type (Type(..))
import Utils (Safe(..))
import VMData (Address, addrToBytes, Vm(..), defaultVM, Any(..))

fromSafe :: Safe a -> a
fromSafe (Error err) = error err
fromSafe (Value a) = a

pushRegister :: RegisterID -> [Maybe Any] -> [Any] -> Safe ([Maybe Any], [Any])
pushRegister 0 ((Just a):reg) stack = Value (Nothing:reg, a:stack)
pushRegister 0 (Nothing:_) _ = Error "Register empty"
pushRegister x (a:as) stack = pushRegister (x - 1) as stack >>=(\(_reg, _stack) -> Value (a:_reg, _stack)) -- not sure about reg movement
pushRegister _ [] _ = Error "Register out of bounds"

pushValue :: Any -> [Any] -> Safe [Any]
pushValue a stack = Value $ a:stack

popStack :: RegisterID -> [Any] -> [Maybe Any] -> Safe ([Maybe Any], [Any])
popStack 0 (a : as) (_ : rs) = Value  (Just a : rs, as)
popStack x stack (r : rs) = popStack (x - 1) stack rs >>=(\(_reg, _stack) -> Value (r:_reg, _stack))
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

executeInstruction :: AssemblyInstruction -> Vm -> Safe Vm
executeInstruction (PushRegister registerID) (Vm reg cstack bf vstack pc) = pushRegister registerID reg vstack >>=(\(_reg, _stack) -> Value (Vm _reg cstack bf _stack pc))
executeInstruction (PushValue value) (Vm reg cstack bf vstack pc) = pushValue value vstack >>=(\ _stack -> Value (Vm reg cstack bf _stack pc))
executeInstruction (Pop registerID) (Vm reg cstack bf vstack pc) = popStack registerID vstack reg >>=(\(_reg, _stack) -> Value (Vm _reg cstack bf _stack pc))
executeInstruction (JumpIfTrue move) (Vm reg cstack bf vstack pc) = Value (Vm reg cstack bf vstack (pc + move))
executeInstruction (JumpIfFalse move) (Vm reg cstack bf vstack pc) = Value (Vm reg cstack bf vstack (pc + move))

parseInstruction' :: [Word8] -> Safe (AssemblyInstruction, Int)
parseInstruction' (0x00 : xs) = mapFst PushValue <$> deserializeTypeAndValue xs
parseInstruction' (0x01 : reg : _) = Value (PushRegister reg, 2)
parseInstruction' (0x10 : reg : _) = Value (Pop reg, 2)
parseInstruction' (0x20 : xs) = deserializeType xs >>=(\(_type, len, rest) -> deserializeInt xs >>= \(a, len', _) -> Value (Construct _type a, len + len'))
parseInstruction' (0x30 : reg : _) = Value (Test reg, 2)
parseInstruction' (0x40 : byte1 : byte2 : byte3 : byte4 : _) = Value (JumpIfTrue (combineWord32 $ byte1 : [byte2] ++ [byte3] ++ [byte4]), 5)
parseInstruction' (0x50 : byte1 : byte2 : byte3 : byte4 : _) = Value (JumpIfFalse (combineWord32 $ byte1 : [byte2] ++ [byte3] ++ [byte4]), 5)
parseInstruction' (0x60 : xs) = deserializeList T_Char 0 xs [] <&> (\(str, i) -> (map (\(Any (_, a)) -> unsafeCoerce a :: Char) str, i)) <&> mapFst Call
parseInstruction' (0x70 : xs) = mapFst RetValue <$> deserializeTypeAndValue xs
parseInstruction' (0x71 : reg : _) = Value (RetRegister reg, 2)
parseInstruction' (0x80 : reg : xs) = mapFst (MovValue reg) <$> deserializeTypeAndValue xs
parseInstruction' (0x81 : reg1 : reg2 : _) = Value (MovRegister reg1 reg2, 3)
parseInstruction' (0x90 : xs) = mapFst OutValue <$> deserializeTypeAndValue xs
parseInstruction' (0x91 : reg : _) = Value (RetRegister reg, 2)
parseInstruction' (_ : xs) = parseInstruction' xs
parseInstruction' [] = Error endOfFile -- maybe special handling to return 0 instead of 1 when  end of file, because it's totally normal behavior

parseInstruction :: Vm -> [Word8] -> Safe Vm
parseInstruction vm bytes = parseInstruction' bytes >>=(\(instruction, movement) -> case executeInstruction instruction vm of
    Error err -> Error err
    Value (Vm _reg _cstack _bf _vstack _pc) -> Value (Vm _reg _cstack _bf _vstack (_pc + u32 movement))) --

-- Missing parseFile command between parse instruction && mainVM

mainVM :: FilePath -> IO ()
mainVM path = do
    file <- readBinary path
    case readSymbolTable file of
        Error err -> die err
        Value (table, rest) -> case parseInstruction defaultVM rest of
            Error err' -> die err'
            Value (Vm _reg _cstack _bf _vstack _pc) -> print _pc
            -- writeBinary "output.txt"

