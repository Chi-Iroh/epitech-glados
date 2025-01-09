module BooleanOperator(booleanBuiltins) where
import Evaluate(evaluateAST1, toNumber, Symbol(BackendSymbol), Symbols)
import AST (AST(..), Call(..), MainAST(..))
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
booleanBuiltins = [ BackendSymbol ("==", astComparisonOp "==" (==))
            ,   BackendSymbol ("!=", astComparisonOp "!=" (/=))
            ,   BackendSymbol ("<", astComparisonOp "<" (<))
            ,   BackendSymbol (">", astComparisonOp ">" (>))
            ,   BackendSymbol ("<=", astComparisonOp "<=" (<=))
            ,   BackendSymbol (">=", astComparisonOp ">=" (>=))
            ,   BackendSymbol ("!", astNot)
            ,   BackendSymbol ("&&", astBoolOperations "&&" (&&))
            ,   BackendSymbol ("||", astBoolOperations "||" (||))
            ,   BackendSymbol ("!&", astBoolOperations "!=" nand)
            ,   BackendSymbol ("!|", astBoolOperations "!=" nor)
            ,   BackendSymbol (":|", astBoolOperations "!=" xor)
            ,   BackendSymbol ("!:", astBoolOperations "!=" xnor)
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

astBoolOperations' :: String -> (Bool -> Bool -> Bool) -> [AST] -> Safe AST
astBoolOperations' _ f [ASTBool a, ASTBool b] = Value $ ASTBool (f a b)
astBoolOperations' name _ args = Error ("Bad arguments when attempting to call " ++ name ++ " ! Expected 2 boolean but got " ++ show args ++ " !")

astBoolOperations :: String -> (Bool -> Bool -> Bool) -> Symbols -> [AST] -> Safe AST
astBoolOperations name f symbols args = mapM (fst . evaluateAST1 symbols) args >>= astBoolOperations' name f

astNot' :: [AST] -> Safe AST
astNot' [ASTBool a] = Value $ ASTBool $ not a
astNot' args = Error ("Bad arguments when attempting to call '!' ! Expected a boolean but got " ++ show args ++ " !")

astNot :: Symbols -> [AST] -> Safe AST
astNot symbols args = mapM (fst . evaluateAST1 symbols) args >>= astNot'