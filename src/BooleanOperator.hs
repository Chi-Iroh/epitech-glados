module BooleanOperator(booleanBuiltins) where

import Any
import DataBuiltins (Symbols, BuiltinsSymbol(BackendBuiltins))
import Utils (Safe(..))

nand :: Bool -> Bool -> Bool
nand a b = not $ a && b

nor :: Bool -> Bool -> Bool
nor a b = not $ a || b

xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

xnor :: Bool -> Bool -> Bool
xnor a b = not $ xor a b

booleanBuiltins :: Symbols
booleanBuiltins = [ BackendBuiltins ("==", pdpBinaryBuiltin "==" (~==))
            ,   BackendBuiltins ("eq", pdpBinaryBuiltin "eq" (~==))
            ,   BackendBuiltins ("!=", pdpBinaryBuiltin "!=" (~/=))
            ,   BackendBuiltins ("neq", pdpBinaryBuiltin "neq" (~/=))
            ,   BackendBuiltins ("<", pdpBinaryBuiltin "<" (~<))
            ,   BackendBuiltins ("lw", pdpBinaryBuiltin "lw" (~<))
            ,   BackendBuiltins (">", pdpBinaryBuiltin ">" (~>))
            ,   BackendBuiltins ("gt", pdpBinaryBuiltin "gt" (~>))
            ,   BackendBuiltins ("<=", pdpBinaryBuiltin "<=" (~<=))
            ,   BackendBuiltins ("lweq", pdpBinaryBuiltin "lweq" (~<=))
            ,   BackendBuiltins (">=", pdpBinaryBuiltin ">=" (~>=))
            ,   BackendBuiltins ("gteq", pdpBinaryBuiltin "gteq" (~>=))
            ,   BackendBuiltins ("!", pdpNot)
            ,   BackendBuiltins ("not", pdpNot)
            ,   BackendBuiltins ("&&", pdpBoolOperations "&&" (&&))
            ,   BackendBuiltins ("and", pdpBoolOperations "and" (&&))
            ,   BackendBuiltins ("||", pdpBoolOperations "||" (||))
            ,   BackendBuiltins ("or", pdpBoolOperations "pr" (||))
            ,   BackendBuiltins ("!&", pdpBoolOperations "!&" nand)
            ,   BackendBuiltins ("nand", pdpBoolOperations "nand" nand)
            ,   BackendBuiltins ("!|", pdpBoolOperations "!|" nor)
            ,   BackendBuiltins ("nor", pdpBoolOperations "nor" nor)
            ,   BackendBuiltins (":|", pdpBoolOperations ":|" xor)
            ,   BackendBuiltins ("xor", pdpBoolOperations "xor" xor)
            ,   BackendBuiltins ("!:", pdpBoolOperations "!:" xnor)
            ,   BackendBuiltins ("xnor", pdpBoolOperations "xnor" xnor)]
            -- ,   BackendBuiltins ("if", pdpIf)]

pdpBinaryBuiltin :: String -> (Any -> Any -> Safe Any) -> [Any] -> Safe Any
pdpBinaryBuiltin _ f [a, b] = f a b
pdpBinaryBuiltin name _ args = Error ("Bad arguments when attempting to call " ++ name ++ ", can only compare 2 arguments, but got " ++ show (length args) ++ " !")

-- pdpIf' :: Symbols -> [Any] -> Safe Any
-- pdpIf' symbols [(Bool condition), a, b] = if condition then eval a else eval b
--     where eval s = fst (evaluateAny1 symbols s)
-- pdpIf' _ args = Error ("if must be called as 'if <condition as boolean> <a> <b>', but got args " ++ show args)

-- pdpIf :: Symbols -> [Any] -> Safe Any
-- pdpIf symbols [a, b, c] = (fst $ evaluateAny1 symbols a) >>= (\a' -> pdpIf' symbols [a', b, c])
-- pdpIf _ args = Error ("if must be called with 3 arguments, but got " ++ show (length args))

pdpBoolOperations :: String -> (Bool -> Bool -> Bool) -> [Any] -> Safe Any
pdpBoolOperations _ f [Bool a, Bool b] = Value $ Bool (f a b)
pdpBoolOperations name _ args = Error ("Bad arguments when attempting to call " ++ name ++ " ! Expected 2 boolean but got " ++ show args ++ " !")

pdpNot :: [Any] -> Safe Any
pdpNot [Bool a] = Value $ Bool $ not a
pdpNot args = Error ("Bad arguments when attempting to call '!' ! Expected a boolean but got " ++ show args ++ " !")
