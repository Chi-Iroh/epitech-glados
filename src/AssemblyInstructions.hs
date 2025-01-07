module AssemblyInstructions where

import Data.Word (Word8)
import Serialize
import Type (Type(..))
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

serialize :: a -> [Word8]
serialize _ = []

assemble :: AssemblyInstruction -> [Word8]
assemble (PushValue (Any (_type, val))) = [0x00] ++ serializeType _type ++ serialize val -- 0x00 : 1st nibble = instruction ID, 2nd nibble = addressing mode
assemble (PushRegister reg) = [0x01, reg]
assemble (Pop reg) = [0x10, reg]
assemble (Test reg) = [0x20, reg]
assemble (JumpIfTrue addr) = [0x30] ++ addrToBytes addr
assemble (JumpIfFalse addr) = [0x40] ++ addrToBytes addr
assemble (Call name) = [0x50] ++ serialize name
assemble (RetValue (Any (_type, val))) = [0x60] ++ serializeType _type ++ serialize val
assemble (RetRegister reg) = [0x61, reg]
assemble (MovValue dest (Any (_type, val))) = [0x70, dest] ++ serializeType _type ++ serialize val
assemble (MovRegister dest src) = [0x71, dest, src]
assemble (OutValue (Any (_type, val))) = [0x80] ++ serializeType _type ++ serialize val
assemble (OutRegister reg) = [0x81, reg]
