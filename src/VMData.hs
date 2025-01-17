{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
module VMData where

import Data.Word (Word8, Word32)
import Text.Printf (printf)
import Bits (splitWord32)
import Serialize (Serializable(..))
import Type (Type(..))
import BinaryIO (readBinary)

data Any = forall a. (Serializable a, Show a) => Any (Type, a)
type Address = Word32

instance Serializable Any where
    serialize (Any (_, a)) = serialize a

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

defaultVM :: Vm
defaultVM = Vm {
    _registers = replicate 16 Nothing,
    _callStack = [],
    _bf = Nothing,
    _valueStack = [],
    _pc = 0
}

addrToBytes :: Address -> [Word8]
addrToBytes = splitWord32

