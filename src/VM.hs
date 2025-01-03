{-# LANGUAGE ExistentialQuantification #-}
module VM where

import Data.Word (Word8, Word32)
import Bits (splitWord32)
import Type (Type(..))

data Any = forall a. Any (Type, a)
type Address = Word32

data VM = VM {
    _registers :: [Any],    -- 16 registers
    _callStack :: [Int],    -- call stack for function calls
    _bf :: Maybe Bool,      -- boolean flag for branching
    _valueStack :: [Any],   -- value stack (where args are pushed)
    _pc :: Address          -- position of current opcode
}

addrToBytes :: Address -> [Word8]
addrToBytes = splitWord32