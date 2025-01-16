{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
module VMData where

import Data.Word (Word8, Word32)
import Text.Printf (printf)
import Bits (splitWord32)
import Type (Type(..))
import BinaryIO (readBinary)

data Any = forall a. Show a => Any (Type, a)
type Address = Word32

instance Show Any where
    show :: Any -> String
    show (Any (type', val)) = printf "Any (%s, %s)" (show type') (show val)

data Vm = Vm {
    _registers :: [Maybe Any],    -- 16 registers
    _callStack :: [Int],    -- call stack for function calls
    _bf :: Maybe Bool,      -- boolean flag for branching
    _valueStack :: [Any],   -- value stack (where args are pushed)
    _pc :: Address          -- position of current opcode
}

addrToBytes :: Address -> [Word8]
addrToBytes = splitWord32

