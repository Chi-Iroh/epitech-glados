module BooleanOperator(booleanBuiltins) where
import Utils (Safe(..))
import VMData(Any(..))
import DataBuiltins (Symbols, BuiltinsSymbol(BackendBuiltins))

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
booleanBuiltins = [ BackendBuiltins ("==", pdpComparisonOp "==" (==))
            ,   BackendBuiltins ("eq", pdpComparisonOp "eq" (==))
            ,   BackendBuiltins ("!=", pdpComparisonOp "!=" (/=))
            ,   BackendBuiltins ("neq", pdpComparisonOp "neq" (/=))
            ,   BackendBuiltins ("<", pdpNumberComparison "<" (<))
            ,   BackendBuiltins ("lw", pdpNumberComparison "lw" (<))
            ,   BackendBuiltins (">", pdpNumberComparison ">" (>))
            ,   BackendBuiltins ("gt", pdpNumberComparison "gt" (>))
            ,   BackendBuiltins ("<=", pdpNumberComparison "<=" (<=))
            ,   BackendBuiltins ("lweq", pdpNumberComparison "lweq" (<=))
            ,   BackendBuiltins (">=", pdpNumberComparison ">=" (>=))
            ,   BackendBuiltins ("gteq", pdpNumberComparison "gteq" (>=))
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

pdpComparisonOp :: String -> (Any -> Any -> Bool) -> [Any] -> Safe Any
pdpComparisonOp name f args@[_, _] = compare' args
    where compare' [a', b'] = Value $ Bool (f a' b')
          compare' args' = Error ("Bad arguments when attempting to call " ++ name ++ " !")
pdpComparisonOp name _ args = Error ("Bad arguments when attempting to call " ++ name ++ ", can only compare 2 arguments, but got " ++ show (length args) ++ " !")

pdpNumberComparison :: String -> (Any -> Any -> Bool) -> [Any] -> Safe Any
pdpNumberComparison _ f [Int a, Int b] = Value $ Bool (f (Int a) (Int b))
pdpNumberComparison _ f [UInt a, UInt b] = Value $ Bool (f (UInt a) (UInt b))
pdpNumberComparison _ f [Char a, Char b] = Value $ Bool (f (Char a) (Char b))
pdpNumberComparison _ f [Float a, Float b] = Value $ Bool (f (Float a) (Float b))
pdpNumberComparison name _ args = Error $ "Bad arguments when attempting to call " ++ name ++ ", can only compare two number of same type, but got " ++ show args ++ " !" 

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
