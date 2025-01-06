module BooleanOperator(booleanBuiltins) where
import Evaluate(evaluateAST1, Symbol, Symbols)
import AST (AST(..), Call(..), MainAST(..))
import Data.List (singleton)
import Utils (Safe(..))
import Type

booleanBuiltins :: Symbols
booleanBuiltins = [ BackendSymbol (">", astComparisonOp ">" (>))
            ,   BackendSymbol (">=", astComparisonOp ">=" (>=))
            ,   BackendSymbol ("<", astComparisonOp "<" (<))
            ,   BackendSymbol ("<=", astComparisonOp "<=" (<=))
            ,   BackendSymbol ("eq?", astComparisonOp "eq?" (==))
            ,   BackendSymbol ("==", astComparisonOp "==" (==))
            ,   BackendSymbol ("if", astIf)]

astComparisonOp' :: String -> (Int -> Int -> Bool) -> [AST] -> Safe AST
astComparisonOp' name f args@[_, _] = mapM toNumber args >>= compare'
    where compare' [a', b'] = Value $ ASTBool (f a' b')
          compare' args' = Error ("Bad arguments when attempting to call " ++ name ++ ", can only compare booleans and integers, but got " ++ show args' ++ " !")
astComparisonOp' name _ args = Error ("Bad arguments when attempting to call " ++ name ++ ", can only compare 2 arguments, but got " ++ show (length args) ++ " !")

astComparisonOp :: String -> (Int -> Int -> Bool) -> Symbols -> [AST] -> Safe AST
astComparisonOp name f symbols args = mapM (fst . evaluateAST1 symbols) args >>= astComparisonOp' name f

astIf' :: Symbols -> [AST] -> Safe AST
astIf' symbols [(ASTBool condition), a, b] = if condition then eval a else eval b
    where eval s = fst (evaluateAST1 symbols s)
astIf' _ args = Error ("if must be called as 'if <condition as boolean> <a> <b>', but got args " ++ show args)

astIf :: Symbols -> [AST] -> Safe AST
astIf symbols [a, b, c] = (fst $ evaluateAST1 symbols a) >>= (\a' -> astIf' symbols [a', b, c])
astIf _ args = Error ("if must be called with 3 arguments, but got " ++ show (length args))
