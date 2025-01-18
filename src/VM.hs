module VM where

import Data.Word (Word8, Word32)

import Any (Any)
import Bits (splitWord32)

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