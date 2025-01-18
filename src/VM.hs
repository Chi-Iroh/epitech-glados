module VM where

import Data.Functor ((<&>))
import Data.Word (Word8, Word32)
import Data.Typeable (typeOf)
import System.Exit (die)
import Text.Printf (printf)
import Unsafe.Coerce (unsafeCoerce)

import AssemblyInstructions (AssemblyInstruction(..), RegisterID)
import BinaryIO (readBinary, writeBinary)
import Bits (splitWord32, combineWord32, u32)
import Deserialize (deserialize, deserializeTypeAndValue, deserializeList, deserializeType, deserializeInt)
import SymbolTable (readSymbolTable, SymbolTable)
import Serialize (Serializable, serialize)
import Type (Type(..))
import Utils (Safe(..))
import VMData (Address, addrToBytes, Vm(..), defaultVM, Any(..))

fromSafe :: Safe a -> a
fromSafe (Error err) = error err
fromSafe (Value a) = a

imap :: (Int -> a -> b) -> [a] -> [b]
imap f arr = map (\(i, a) -> f i a) (zip [0..] arr)

setAt :: Int -> a -> [a] -> [a]
setAt _ _ [] = []
setAt 0 a (_ : xs) = a : xs
setAt n a (x : xs) = x : setAt (n - 1) a xs

mapAt :: (a -> a) -> Int -> [a] -> [a]
mapAt f i = imap (\i' a -> if i == i' then f a else a)

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
constructList 0 _type stack newlist = Value $ Any (T_List _type, newlist):stack 
constructList x _type (a:as) newlist = constructList (x - 1) _type as $ a:newlist
constructList x _type [] _ = Error "Not enough value in stack to perform construct"

constructTupple :: [Any] -> Type -> Type -> Safe [Any]
constructTupple (a : b : as) typeA typeB = Value $ Any (T_Tuple (typeA, typeB), (a, b)):as
constructTupple [a] _ _ = Error "Not enough value in stack to perform construct"

construct :: Type -> Int -> [Any] -> Safe [Any]
construct (T_List _type) size stack = constructList size _type stack []
construct (T_Tuple (typeA, typeB)) 2 stack = constructTupple stack typeA typeB
construct (T_Tuple (_, _)) size _ = Error $ "Tried to construct tupple of size" ++ show size ++ "but tupples can only be of size 2"

moveValue :: RegisterID -> [Maybe Any] -> Any -> Safe [Maybe Any]
moveValue 0 (_ : rs) a = Value (Just a : rs)
moveValue x (r:rs) a = moveValue (x - 1) rs a >>=(\_reg -> Value (r:_reg))

moveRegister :: RegisterID -> RegisterID -> [Maybe Any] -> [Maybe Any] -> Safe [Maybe Any]
moveRegister 0 0 (_ : rs) (r : _) = Value (r : rs)
moveRegister 0 0 _ (Nothing : _) = Error "Source register empty"
moveRegister 0 y reg1 (r': rs') = moveRegister 0 (y - 1) reg1 rs'
moveRegister x 0 (r : rs) reg2 = moveRegister (x - 1) 0 rs reg2 >>=(\_reg -> Value (r:_reg))
moveRegister x y (r : rs) (r': rs') = moveRegister (x - 1) (y - 1) rs rs' >>=(\_reg -> Value (r:_reg))

testReg :: Maybe Any -> Safe Bool
testReg (Just (Any(T_Bool, val)))
                    | unsafeCoerce val :: Bool = Value True
                    | otherwise = Value False
testReg (Just r) = Error "Register doesn't contain a boolean value"
testReg Nothing = Error "Register is empty"

jumpTrue :: Address -> Maybe Bool -> Int -> Safe Address
jumpTrue _ Nothing _ = Error "BF not set, please use test before conditional jump"
jumpTrue _ (Just False) _ = Value 0
jumpTrue addr (Just True) len = Value $ addr - u32 len

jumpFalse :: Address -> Maybe Bool -> Int -> Safe Address
jumpFalse _ Nothing _ = Error "BF not set, please use test before conditional jump"
jumpFalse _ (Just True)_  = Value 0
jumpFalse addr (Just False) len = Value $ addr - u32 len

call :: String -> SymbolTable -> Safe Address
call str ((str', address):ts)
                | str == str' = Value address
                | otherwise = call str ts 
call str [] = Error $ "Can't find function " ++ str

returnRegister :: [Address] -> Maybe Any -> [Any] -> Safe ([Address], [Any], Address)
returnRegister (a:as) (Just val) stack = Value (as, val : stack, a)
returnRegister _ Nothing _ = Error "Empty register in return call"
returnRegister [] _ _ = Error "Can't return in highest degree function"

returnValue :: [Address] -> Any -> [Any] -> Safe ([Address], [Any], Address)
returnValue (a:as) val stack = Value (as, val : stack, a)
returnValue [] _ _ = Error "Can't return in highest degree function"

endOfFile :: String
endOfFile = "End of file"

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

executeInstruction :: AssemblyInstruction -> SymbolTable -> Vm -> Int -> Safe Vm
executeInstruction (PushRegister registerID) _ (Vm (reg:rs) cstack bf vstack pc) _ = pushRegister (reg !! fromIntegral registerID) vstack >>=(\_stack -> Value (Vm (reg:rs) cstack bf _stack pc))
executeInstruction (PushValue value) _ (Vm (reg:rs) cstack bf vstack pc) _ = pushValue value vstack >>=(\ _stack -> Value (Vm (reg:rs) cstack bf _stack pc))
executeInstruction (Pop registerID) _ (Vm (reg:rs) cstack bf vstack pc) _ = popStack registerID vstack reg >>=(\(_reg, _stack) -> Value (Vm (_reg:rs) cstack bf _stack pc))
executeInstruction (Construct _type _size) _ (Vm (reg:rs) cstack bf vstack pc) _ = construct _type _size vstack >>=(\_stack -> Value $ Vm (reg:rs) cstack bf _stack pc)
executeInstruction (Test registerID) _ (Vm (reg:rs) cstack bf vstack pc) _ = testReg (reg !! fromIntegral registerID) >>=(\_bf -> Value (Vm (reg:rs) cstack (Just _bf) vstack pc))
executeInstruction (JumpIfTrue addr) _ (Vm (reg:rs) cstack bf vstack pc) len = jumpTrue addr bf len >>=(\move -> Value (Vm (reg:rs) cstack bf vstack (pc + move)))
executeInstruction (JumpIfFalse addr) _ (Vm (reg:rs) cstack bf vstack pc) len = jumpFalse addr bf len >>=(\move -> Value (Vm (reg:rs) cstack bf vstack (pc + move)))
executeInstruction (Call str) table (Vm reg cstack bf vstack pc) len = call str table >>=(\_address -> Value (Vm (replicate 16 Nothing : reg) (pc:cstack) bf vstack (_address - u32 len)))
executeInstruction (RetRegister registerID) _ (Vm (reg:rs) cstack bf vstack pc) len = returnRegister cstack (reg !! fromIntegral registerID) vstack >>=(\(_cstack, _vstack, _address) -> Value (Vm rs _cstack bf _vstack (_address - u32 len)))
executeInstruction (RetValue value) _ (Vm (reg:rs) cstack bf vstack pc) len = returnValue cstack value vstack >>=(\(_cstack, _vstack, _address) -> Value (Vm rs _cstack bf _vstack (_address - u32 len)))
executeInstruction (MovRegister register1 register2) _ (Vm (reg:rs) cstack bf vstack pc) _ = moveRegister register1 register2 reg reg >>=(\_reg -> Value (Vm (_reg:rs) cstack bf vstack pc))
executeInstruction (MovValue registerID value) _ (Vm (reg:rs) cstack bf vstack pc) _ =  moveValue registerID reg value >>=(\_reg -> Value (Vm (_reg:rs) cstack bf vstack pc))

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

parseInstruction :: Vm -> [Word8] -> SymbolTable -> Safe Vm
parseInstruction vm bytes table = parseInstruction' bytes >>=(\(instruction, movement) -> case executeInstruction instruction table vm movement of
    Error err -> Error err
    Value (Vm _reg _cstack _bf _vstack _pc) -> Value (Vm _reg _cstack _bf _vstack (_pc + u32 movement))) --

-- Missing parseFile command between parse instruction && mainVM

mainVM :: FilePath -> IO ()
mainVM path = do
    file <- readBinary path
    case readSymbolTable file of
        Error err -> die err
        Value (table, rest) -> case parseInstruction defaultVM rest table of
            Error err' -> die err'
            Value (Vm _reg _cstack _bf _vstack _pc) -> print _pc
            -- writeBinary "output.txt"

-- reg >> [[Any]], prend toujours le head