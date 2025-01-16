{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
module VM where

import Data.Word (Word8, Word32)
import Text.Printf (printf)

import Bits (splitWord32)
import Serialize (Serializable, serialize)
import Type (Type(..))

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