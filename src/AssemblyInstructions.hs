{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
module AssemblyInstructions (RegisterID, AssemblyInstruction(..), astToAny, assemble, toAssemblyValueInstruction, concatMapM) where

import Data.Functor ((<&>))
import Data.Word (Word8)

import Any (Any(..), AnyAssembly(..), makeAny, anyType)
import AST (AST(..))
import Bits (u32)
import Hex (showHex32)
import Serialize
import Type (Type(..))
import Utils (Safe(..), mapFst, concatMapM)
import VMData (Address, addrToBytes)

type RegisterID = Word8

data AssemblyInstruction =  PushRegister RegisterID             |
                            PushValue Any                       |
                            Pop RegisterID                      |
                            Test RegisterID                     |
                            Jump Address                        |
                            JumpIfTrue Address                  |
                            JumpIfFalse Address                 |
                            Call String                         |
                            RetRegister RegisterID              |
                            RetValue Any                        |
                            Ret                                 |
                            MovRegister RegisterID RegisterID   |
                            MovValue RegisterID Any             |
                            OutRegister RegisterID              |
                            OutValue Any                        |
                            Construct Type Int

instance Show AssemblyInstruction where
    show :: AssemblyInstruction -> String
    show (PushRegister reg) = "push r" ++ show reg
    show (PushValue val) = "push " ++ show (AnyAssembly val)
    show (Pop reg) = "pop r" ++ show reg
    show (Construct _type n) = "construct " ++ show _type ++ " " ++ show n
    show (Test reg) = "test r" ++ show reg
    show (Jump addr) = "jmp " ++ showHex32 addr
    show (JumpIfTrue addr) = "jt " ++ showHex32 addr ++ " (number of instructions, not actual assembled bytes)"
    show (JumpIfFalse addr) = "jf " ++ showHex32 addr ++ " (number of instructions, not actual assembled bytes)"
    show (Call symbol) = "call " ++ symbol
    show (RetRegister reg) = "ret r" ++ show reg
    show (RetValue val) = "ret " ++ show (AnyAssembly val)
    show Ret = "ret"
    show (MovRegister dest src) = "mov r" ++ show dest ++ ", " ++ show src
    show (MovValue dest val) = "mov r" ++ show dest ++ ", " ++ show (AnyAssembly val)
    show (OutRegister reg) = "out r" ++ show reg
    show (OutValue val) = "out " ++ show (AnyAssembly val)

astToAny :: AST -> Safe Any
astToAny (ASTBool b) = makeAny T_Bool b
astToAny (ASTChar c) = makeAny T_Char c
astToAny (ASTInt n) = makeAny T_Int n
astToAny (ASTUInt n) = makeAny T_UInt n
astToAny (ASTFloat f) = makeAny T_Float f
astToAny (ASTString str) = makeAny T_String str
astToAny (ASTArray xs) = mapM astToAny xs <&> Array
astToAny (ASTTuple (a, b)) = liftA2 (curry Tuple) (astToAny a) (astToAny b)
astToAny a = Error ("astToAny: Invalid argument : '" ++ show a ++ "'")

toAssemblyValueInstruction :: (Any -> AssemblyInstruction) -> AST -> Safe AssemblyInstruction
toAssemblyValueInstruction instruction ast = fmap instruction (astToAny ast)

assemble1 :: AssemblyInstruction -> Safe [Word8]
assemble1 (PushValue val) = liftA2 (++) (anyType val >>= serializeType) (serialize val) <&> ([0x00] ++) -- 0x00 : 1st nibble = instruction ID, 2nd nibble = addressing mode
assemble1 (PushRegister reg) = Value [0x01, reg]
assemble1 (Pop reg) = Value [0x10, reg]
assemble1 (Construct _type n) = liftA2 (++) (serializeType _type) (serialize n) <&> ([0x20] ++)
assemble1 (Test reg) = Value [0x30, reg]
assemble1 (Call name) = concatMapM serialize name <&> (\bytes -> [0x60] ++ bytes ++ [0x00])
assemble1 (RetValue val) = liftA2 (++) (anyType val >>= serializeType) (serialize val) <&> ([0x70] ++)
assemble1 (RetRegister reg) = Value [0x71, reg]
assemble1 Ret = Value [0x72]
assemble1 (MovValue dest val) = liftA2 (++) (anyType val >>= serializeType) (serialize val) <&> ([0x80, dest] ++)
assemble1 (MovRegister dest src) = Value [0x81, dest, src]
assemble1 (OutValue val) = liftA2 (++) (anyType val >>= serializeType) (serialize val) <&> ([0x90] ++)
assemble1 (OutRegister reg) = Value [0x91, reg]
assemble1 a = Error ("assemble1: Instruction " ++ show a ++ " not implemented !")

assemble :: [AssemblyInstruction] -> Safe [Word8]
assemble (JumpIfTrue size : xs) = liftA2 (\falseCode' rest' -> [0x40] ++ addrToBytes (u32 $ length falseCode') ++ falseCode' ++ rest') falseCode (assemble rest)
    where (falseCode, rest) = mapFst (concatMapM assemble1) $ splitAt (fromIntegral size) xs
assemble (JumpIfFalse size : xs) = liftA2 (\trueCode' rest' -> [0x50] ++ addrToBytes (u32 $ length trueCode') ++ trueCode' ++ rest') trueCode (assemble rest)
    where (trueCode, rest) = mapFst (concatMapM assemble1) $ splitAt (fromIntegral size) xs
assemble (Jump size : xs) = liftA2 (\code' rest' -> [0xA0] ++ addrToBytes (u32 $ length code') ++ code' ++ rest') code (assemble rest)
    where (code, rest) = mapFst (concatMapM assemble1) $ splitAt (fromIntegral size) xs
assemble (x : xs) = liftA2 (++) (assemble1 x) (assemble xs)
assemble [] = Value []
