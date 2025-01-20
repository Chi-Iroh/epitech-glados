{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
module AssemblyInstructions (RegisterID, AssemblyInstruction(..), toAny, assemble, toAssemblyValueInstruction, concatMapM) where

import Data.Functor ((<&>))
import Data.Typeable (typeOf, Typeable)
import Data.Word (Word8)
import Debug.Trace (traceShow)
import Debug.Trace (traceShowId)
import Unsafe.Coerce (unsafeCoerce)

import AST (AST(..), getTypeAST)
import Bits (u32)
import Hex (showHex32)
import Serialize
import Type (Type(..))
import Utils (Safe(..), mapFst, concatMapM)
import VMData (Any(..), Address, addrToBytes)

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
                            OutValue Any                        |
                            Construct Type Int

instance Show AssemblyInstruction where
    show :: AssemblyInstruction -> String
    show (PushRegister reg) = "push r" ++ show reg
    show (PushValue (Any (_type, a))) = "push " ++ show _type ++ " " ++ show a
    show (Pop reg) = "pop r" ++ show reg
    show (Construct _type n) = "construct " ++ show _type ++ " " ++ show n
    show (Test reg) = "test r" ++ show reg
    show (JumpIfTrue addr) = "jt " ++ showHex32 addr ++ " (number of instructions, not actual assembled bytes)"
    show (JumpIfFalse addr) = "jf " ++ showHex32 addr ++ " (number of instructions, not actual assembled bytes)"
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
    where types = liftA2 (,) (getTypeAST a) (getTypeAST b)
          values = liftA2 (,) (toAny a) (toAny b)
toAny list@(ASTArray xs) = liftA2 (\type' values' -> Any (type', map fromAny values')) (getTypeAST list) values
    where values = traceVal "values" (mapM toAny xs)
toAny a = error ("toAny: Invalid argument : '" ++ show a ++ "'")
-- toAny a = Error ("toAny: Invalid argument : '" ++ show a ++ "'")

toAssemblyValueInstruction :: (Any -> AssemblyInstruction) -> AST -> Safe AssemblyInstruction
toAssemblyValueInstruction instruction ast = fmap instruction (toAny ast)
-- toAssemblyValueInstruction instruction (ASTBool b) = Value $ instruction (Any (T_Bool, b))
-- toAssemblyValueInstruction instruction (ASTChar c) = Value $ instruction (Any (T_Char, c))
-- toAssemblyValueInstruction instruction (ASTInt n) = Value $ instruction (Any (T_Int, n))
-- toAssemblyValueInstruction instruction (ASTUInt n) = Value $ instruction (Any (T_UInt, n))
-- toAssemblyValueInstruction instruction (ASTFloat f) = Value $ instruction (Any (T_Float, f))
-- toAssemblyValueInstruction instruction (ASTString str) = Value $ instruction (Any (T_String, str))
-- toAssemblyValueInstruction instruction (ASTTuple (a, b)) = liftA2 (getTypeAST tuple) (toAny ) instruction . Any . (, tuple) <$> 
-- toAssemblyValueInstruction instruction (ASTList x) = instruction . Any . (, x) <$> getTypeAST x
-- toAssemblyValueInstruction _ _ = Error "Invalid argument !"

assemble1 :: AssemblyInstruction -> Safe [Word8]
assemble1 (PushValue (Any (_type, val))) = liftA2 (++) (serializeType _type) (serialize val) <&> ([0x00] ++) -- 0x00 : 1st nibble = instruction ID, 2nd nibble = addressing mode
assemble1 (PushRegister reg) = Value [0x01, reg]
assemble1 (Pop reg) = Value [0x10, reg]
assemble1 (Construct _type n) = liftA2 (++) (serializeType _type) (serialize n) <&> ([0x20] ++)
assemble1 (Test reg) = Value [0x30, reg]
assemble1 (Call name) = concatMapM serialize name <&> (\bytes -> [0x60] ++ bytes ++ [0x00])
assemble1 (RetValue (Any (_type, val))) = liftA2 (++) (serializeType _type) (serialize val) <&> ([0x70] ++)
assemble1 (RetRegister reg) = Value [0x71, reg]
assemble1 (MovValue dest (Any (_type, val))) = liftA2 (++) (serializeType _type) (serialize val) <&> ([0x80, dest] ++)
assemble1 (MovRegister dest src) = Value [0x81, dest, src]
assemble1 (OutValue (Any (_type, val))) = liftA2 (++) (serializeType _type) (serialize val) <&> ([0x90] ++)
assemble1 (OutRegister reg) = Value [0x91, reg]
assemble1 a = Error ("assemble1: Instruction " ++ show a ++ " not implemented !")

assemble' :: [AssemblyInstruction] -> Safe [Word8]
assemble' (JumpIfTrue size : xs) = liftA2 (\falseCode' rest' -> [0x40] ++ addrToBytes (u32 $ length falseCode') ++ falseCode' ++ rest') falseCode (assemble' rest)
    where (falseCode, rest) = mapFst (concatMapM assemble1) $ splitAt (fromIntegral size) xs
assemble' (JumpIfFalse size : xs) = liftA2 (\trueCode' rest' -> [0x50] ++ addrToBytes (u32 $ length trueCode') ++ trueCode' ++ rest') trueCode (assemble' rest)
    where (trueCode, rest) = mapFst (concatMapM assemble1) $ splitAt (fromIntegral size) xs
assemble' (x : xs) = liftA2 (++) (assemble1 x) (assemble' xs)
assemble' [] = Value []

assemble :: [AssemblyInstruction] -> Safe [Word8]
assemble = assemble' . (map traceShowId)