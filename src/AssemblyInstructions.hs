{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
module AssemblyInstructions (RegisterID, AssemblyInstruction(..), toAny, assemble, toAssemblyValueInstruction) where

import Data.Typeable (typeOf, Typeable)
import Debug.Trace (traceShow)
import Data.Word (Word8)
import Debug.Trace (traceShowId)
import Unsafe.Coerce (unsafeCoerce)

import AST (AST(..), getType)
import Hex (showHex32)
import Serialize
import Type (Type(..))
import Utils (Safe(..))
import VM (Any(..), Address, addrToBytes)

type RegisterID = Word8

data AssemblyInstruction =  PushRegister RegisterID             |
                            PushValue Any                       |
                            Pop RegisterID                      |
                            Test RegisterID                     |
                            JumpIfTrue Address                  |
                            JumpIfFalse Address                 |
                            Call String                         |
                            RetRegister RegisterID              |
                            RetValue Any                        |
                            MovRegister RegisterID RegisterID   |
                            MovValue RegisterID Any             |
                            OutRegister RegisterID              |
                            OutValue Any

instance Show AssemblyInstruction where
    show :: AssemblyInstruction -> String
    show (PushRegister reg) = "push r" ++ show reg
    show (PushValue (Any (_type, a))) = "push " ++ show _type ++ " " ++ show a
    show (Pop reg) = "pop r" ++ show reg
    show (Test reg) = "test r" ++ show reg
    show (JumpIfTrue addr) = "jt " ++ showHex32 addr
    show (JumpIfFalse addr) = "jf " ++ showHex32 addr
    show (Call symbol) = "call " ++ symbol
    show (RetRegister reg) = "ret r" ++ show reg
    show (RetValue (Any (_type, a))) = "ret " ++ show _type ++ " " ++ show a
    show (MovRegister dest src) = "mov r" ++ show dest ++ ", " ++ show src
    show (MovValue dest (Any (_type, a))) = "mov r" ++ show dest ++ ", " ++ show _type ++ " " ++ show a
    show (OutRegister reg) = "out r" ++ show reg
    show (OutValue (Any (_type, a))) = "out " ++ show _type ++ " " ++ show a

traceVal :: (Typeable a, Show a) => String -> a -> a
traceVal msg a = traceShow (msg ++ " = " ++ show a ++ " :: " ++ show (typeOf a)) a

fromAny :: Any -> Int
fromAny (Any (_, a)) = unsafeCoerce a

toAny :: AST -> Safe Any
toAny (ASTBool b) = Value (Any (T_Bool, b))
toAny (ASTChar c) = Value (Any (T_Char, c))
toAny (ASTInt n) = Value (Any (T_Int, n))
toAny (ASTUInt n) = Value (Any (T_UInt, n))
toAny (ASTFloat f) = Value (Any (T_Float, f))
toAny (ASTString str) = Value (Any (T_String, str))
toAny (ASTTuple (a, b)) = liftA2 (\types' values' -> Any (T_Tuple types', values')) types values
    where types = liftA2 (,) (getType a) (getType b)
          values = liftA2 (,) (toAny a) (toAny b)
toAny list@(ASTList xs) = liftA2 (\type' values' -> Any (type', map fromAny values')) (getType list) values
    where values = traceVal "values" (mapM toAny xs)
toAny a = Error ("toAny: Invalid argument : '" ++ show a ++ "'")

toAssemblyValueInstruction :: (Any -> AssemblyInstruction) -> AST -> Safe AssemblyInstruction
toAssemblyValueInstruction instruction ast = fmap instruction (toAny ast)
-- toAssemblyValueInstruction instruction (ASTBool b) = Value $ instruction (Any (T_Bool, b))
-- toAssemblyValueInstruction instruction (ASTChar c) = Value $ instruction (Any (T_Char, c))
-- toAssemblyValueInstruction instruction (ASTInt n) = Value $ instruction (Any (T_Int, n))
-- toAssemblyValueInstruction instruction (ASTUInt n) = Value $ instruction (Any (T_UInt, n))
-- toAssemblyValueInstruction instruction (ASTFloat f) = Value $ instruction (Any (T_Float, f))
-- toAssemblyValueInstruction instruction (ASTString str) = Value $ instruction (Any (T_String, str))
-- toAssemblyValueInstruction instruction (ASTTuple (a, b)) = liftA2 (getType tuple) (toAny ) instruction . Any . (, tuple) <$> 
-- toAssemblyValueInstruction instruction (ASTList x) = instruction . Any . (, x) <$> getType x
-- toAssemblyValueInstruction _ _ = Error "Invalid argument !"

dummySerialize :: a -> [Word8]
dummySerialize _ = []

assemble' :: AssemblyInstruction -> [Word8]
assemble' (PushValue (Any (_type, val))) = [0x00] ++ serializeType _type ++ dummySerialize val -- 0x00 : 1st nibble = instruction ID, 2nd nibble = addressing mode
assemble' (PushRegister reg) = [0x01, reg]
assemble' (Pop reg) = [0x10, reg]
assemble' (Test reg) = [0x20, reg]
assemble' (JumpIfTrue addr) = [0x30] ++ addrToBytes addr
assemble' (JumpIfFalse addr) = [0x40] ++ addrToBytes addr
assemble' (Call name) = [0x50] ++ dummySerialize name
assemble' (RetValue (Any (_type, val))) = [0x60] ++ serializeType _type ++ dummySerialize val
assemble' (RetRegister reg) = [0x61, reg]
assemble' (MovValue dest (Any (_type, val))) = [0x70, dest] ++ serializeType _type ++ dummySerialize val
assemble' (MovRegister dest src) = [0x71, dest, src]
assemble' (OutValue (Any (_type, val))) = [0x80] ++ serializeType _type ++ dummySerialize val
assemble' (OutRegister reg) = [0x81, reg]

assemble :: AssemblyInstruction -> [Word8]
assemble = assemble' . traceShowId
