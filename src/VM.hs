module VM (mainVM) where

import Data.Functor ((<&>))
import Data.List (singleton)
import Data.Word (Word8)
import Debug.Trace (traceShowId)
import System.Exit (die)
import Unsafe.Coerce (unsafeCoerce)

import AssemblyInstructions (AssemblyInstruction(..), RegisterID)
import BinaryIO (readBinary)
import Bits (combineWord32, u32)
import Deserialize (deserializeTypeAndValue, deserializeList, deserializeType, deserializeInt, addBytesLen)
import SymbolTable (readSymbolTable, SymbolTable)
import Type (Type(..))
import Utils (Safe(..))
import VMData (Address, Vm(..), defaultVM, Any(..), makeAny)

pushRegister :: Maybe Any -> [Any] -> Safe [Any]
pushRegister (Just a) stack = Value $ a:stack
pushRegister Nothing _ = Error "Register empty"

pushValue :: Any -> [Any] -> Safe [Any]
pushValue a stack = Value $ a:stack

popStack :: RegisterID -> [Any] -> [Maybe Any] -> Safe ([Maybe Any], [Any])
popStack 0 (a : as) (_ : rs) = Value  (Just a : rs, as)
popStack x stack (r : rs) = popStack (x - 1) stack rs >>=(\(_reg, _stack) -> Value (r:_reg, _stack))
popStack _ [] _ = Error "Stack empty"
popStack _ _ [] = Error "Register out of bounds"

constructList :: Int -> Type -> [Any] -> [Any] -> Safe [Any]
constructList 0 _type stack newlist = makeAny (T_List _type) newlist <&> ((++ stack) . singleton)
constructList x _type (a:as) newlist = constructList (x - 1) _type as $ a:newlist
constructList _ _type [] _ = Error "Not enough value in stack to perform construct"

constructTupple :: [Any] -> Type -> Type -> Safe [Any]
constructTupple (a : b : as) typeA typeB = makeAny (T_Tuple (typeA, typeB)) (a, b) <&> ((++ as) . singleton)
constructTupple [_] _ _ = Error "Not enough value in stack to perform construct"
constructTupple _ _ _ = Error "Not enough value in stack to perform construct"

construct :: Type -> Int -> [Any] -> Safe [Any]
construct (T_List _type) size stack = constructList size _type stack []
construct (T_Tuple (typeA, typeB)) 2 stack = constructTupple stack typeA typeB
construct (T_Tuple (_, _)) size _ = Error $ "Tried to construct tupple of size" ++ show size ++ "but tupples can only be of size 2"
construct _ _ _ = Error "Tried to use construct with wrong type"

moveValue :: RegisterID -> [Maybe Any] -> Any -> Safe [Maybe Any]
moveValue 0 (_ : rs) a = Value (Just a : rs)
moveValue x (r:rs) a = moveValue (x - 1) rs a >>=(\_reg -> Value (r:_reg))
moveValue  _ _ _ = Error "moveValue error"

moveRegister :: RegisterID -> RegisterID -> [Maybe Any] -> [Maybe Any] -> Safe [Maybe Any]
moveRegister 0 0 (_ : rs) (r : _) = Value (r : rs)
moveRegister 0 0 _ (Nothing : _) = Error "Source register empty"
moveRegister 0 y reg1 (_: rs') = moveRegister 0 (y - 1) reg1 rs'
moveRegister x 0 (r : rs) reg2 = moveRegister (x - 1) 0 rs reg2 >>=(\_reg -> Value (r:_reg))
moveRegister x y (r : rs) (_: rs') = moveRegister (x - 1) (y - 1) rs rs' >>=(\_reg -> Value (r:_reg))
moveRegister _ _ _ _ = Error "moveRegister error"

testReg :: Maybe Any -> Safe Bool
testReg (Just (Bool val))
                    | unsafeCoerce val :: Bool = Value True
                    | otherwise = Value False
testReg (Just _) = Error "Register doesn't contain a boolean value"
testReg Nothing = Error "Register is empty"

jumpTrue :: Address -> Maybe Bool -> Safe Address
jumpTrue _ Nothing = Error "BF not set, please use test before conditional jump"
jumpTrue _ (Just False) = Value 0
jumpTrue addr (Just True) = Value addr

jumpFalse :: Address -> Maybe Bool -> Safe Address
jumpFalse _ Nothing = Error "BF not set, please use test before conditional jump"
jumpFalse _ (Just True) = Value 0
jumpFalse addr (Just False) = Value addr

call :: String -> SymbolTable -> Safe Address
call str ((str', address):ts)
                | str == str' = Value address
                | otherwise = call str ts
call str [] = Error $ "Can't find function " ++ str

returnRegister :: [Address] -> Maybe Any -> [Any] -> Safe ([Address], [Any], Address)
returnRegister (a:as) (Just val) stack = Value (as, val : stack, a)
returnRegister _ Nothing _ = Error "Empty register in return call"
returnRegister [] _ _ = Error "Not in a function cannot return"

returnValue :: [Address] -> Any -> [Any] -> Safe ([Address], [Any], Address)
returnValue (a:as) val stack = Value (as, val : stack, a)
returnValue [] _ _ = Error "Not in a function cannot return"

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

executeInstruction' :: AssemblyInstruction -> SymbolTable -> Vm -> Safe (Vm, Maybe Any)
executeInstruction' (PushRegister registerID) _ (Vm (reg:rs) cstack bf vstack pc) = pushRegister (reg !! fromIntegral registerID) vstack >>=(\_stack -> Value (Vm (reg:rs) cstack bf _stack pc, Nothing))
executeInstruction' (PushValue value) _ (Vm (reg:rs) cstack bf vstack pc) = pushValue value vstack >>=(\ _stack -> Value (Vm (reg:rs) cstack bf _stack pc, Nothing))
executeInstruction' (Pop registerID) _ (Vm (reg:rs) cstack bf vstack pc) = popStack registerID vstack reg >>=(\(_reg, _stack) -> Value (Vm (_reg:rs) cstack bf _stack pc, Nothing))
executeInstruction' (Construct _type _size) _ (Vm (reg:rs) cstack bf vstack pc) = construct _type _size vstack >>=(\_stack -> Value (Vm (reg:rs) cstack bf _stack pc, Nothing))
executeInstruction' (Test registerID) _ (Vm (reg:rs) cstack _ vstack pc) = testReg (reg !! fromIntegral registerID) >>=(\_bf -> Value (Vm (reg:rs) cstack (Just _bf) vstack pc, Nothing))
executeInstruction' (JumpIfTrue addr) _ (Vm (reg:rs) cstack bf vstack pc) = jumpTrue addr bf >>=(\move -> Value (Vm (reg:rs) cstack bf vstack (pc + move), Nothing))
executeInstruction' (JumpIfFalse addr) _ (Vm (reg:rs) cstack bf vstack pc) = jumpFalse addr bf >>=(\move -> Value (Vm (reg:rs) cstack bf vstack (pc + move), Nothing))
executeInstruction' (Call str) table (Vm reg cstack bf vstack pc)= call str table >>=(\_address -> Value (Vm (replicate 16 Nothing : reg) (pc:cstack) bf vstack _address, Nothing))
executeInstruction' (RetRegister registerID) _ (Vm (reg:rs) cstack bf vstack _) = returnRegister cstack (reg !! fromIntegral registerID) vstack >>=(\(_cstack, _vstack, _address) -> Value (Vm rs _cstack bf _vstack _address, Nothing))
executeInstruction' (RetValue value) _ (Vm (_:rs) cstack bf vstack _)= returnValue cstack value vstack >>=(\(_cstack, _vstack, _address) -> Value (Vm rs _cstack bf _vstack _address, Nothing))
executeInstruction' (MovRegister register1 register2) _ (Vm (reg:rs) cstack bf vstack pc) = moveRegister register1 register2 reg reg >>=(\_reg -> Value (Vm (_reg:rs) cstack bf vstack pc, Nothing))
executeInstruction' (MovValue registerID value) _ (Vm (reg:rs) cstack bf vstack pc) =  moveValue registerID reg value >>=(\_reg -> Value (Vm (_reg:rs) cstack bf vstack pc, Nothing))
executeInstruction' (OutRegister registerID) _ (Vm (reg:rs) cstack bf vstack pc) = Value (Vm (reg:rs) cstack bf vstack pc, reg !! fromIntegral registerID)
executeInstruction' (OutValue value) _ vm = Value (vm, Just value)
executeInstruction' _ _ _ = Error "Instruction not recognized"

executeInstruction :: AssemblyInstruction -> SymbolTable -> Vm -> Safe (Vm, Maybe Any)
executeInstruction instruction = executeInstruction' (traceShowId instruction)

parseInstruction' :: [Word8] -> Safe (AssemblyInstruction, Int)
parseInstruction' (0x00 : xs) = mapFst PushValue <$> addBytesLen 1 <$> deserializeTypeAndValue xs
parseInstruction' (0x01 : reg : _) = Value (PushRegister reg, 2)
parseInstruction' (0x10 : reg : _) = Value (Pop reg, 2)
parseInstruction' (0x20 : xs) = deserializeType xs >>=(\(_type, len, _) -> deserializeInt xs >>= \(a, len', _) -> Value (Construct _type a, len + len'))
parseInstruction' (0x30 : reg : _) = Value (Test reg, 2)
parseInstruction' (0x40 : byte1 : byte2 : byte3 : byte4 : _) = Value (JumpIfTrue (combineWord32 [byte1, byte2, byte3, byte4]), 5)
parseInstruction' (0x50 : byte1 : byte2 : byte3 : byte4 : _) = Value (JumpIfFalse (combineWord32 [byte1, byte2, byte3, byte4]), 5)
parseInstruction' (0x60 : xs) = deserializeList T_Char xs <&> (\((Array str), i, _) -> (map (\(Char c) -> c) str, i)) <&> mapFst Call
parseInstruction' (0x70 : xs) = mapFst RetValue <$> addBytesLen 1 <$> deserializeTypeAndValue xs
parseInstruction' (0x71 : reg : _) = Value (RetRegister reg, 2)
parseInstruction' (0x80 : reg : xs) = mapFst (MovValue reg) <$> addBytesLen 2 <$> deserializeTypeAndValue xs
parseInstruction' (0x81 : reg1 : reg2 : _) = Value (MovRegister reg1 reg2, 3)
parseInstruction' (0x90 : xs) = mapFst OutValue <$> addBytesLen 1 <$> deserializeTypeAndValue (traceShowId xs)
parseInstruction' (0x91 : reg : _) = Value (RetRegister reg, 2)
parseInstruction' _ = Error "No instruction to parse"

movePc :: Int -> Vm -> Vm
movePc increment vm = vm {
    _pc = _pc vm + u32 increment
}

parseInstruction :: Vm -> [Word8] -> SymbolTable -> Safe (Vm, Maybe Any)
parseInstruction vm bytes table = parseInstruction' (traceShowId $ drop (fromIntegral $ _pc vm) bytes) >>=(\(instruction, movement) -> executeInstruction instruction table (movePc movement vm))

parseFile :: Vm -> SymbolTable -> [Word8] -> IO ()
parseFile _ _ [] = print "End of file. VM closing now."
parseFile vm table bytes = case parseInstruction vm bytes table of
                        Error err -> die err
                        Value (_vm, Nothing) -> parseFile _vm table bytes
                        Value (_vm, Just a) -> print a >> parseFile _vm table bytes

mainVM :: FilePath -> IO ()
mainVM path = do
    file <- readBinary path
    case readSymbolTable file of
        Error err -> die err
        Value (table, rest) -> parseFile defaultVM table rest
            -- writeBinary "output.txt"
