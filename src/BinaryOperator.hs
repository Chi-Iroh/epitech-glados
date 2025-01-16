module BinaryOperator(binaryBuiltins) where
import Evaluate(evaluateAST1, Symbol(BackendSymbol), Symbols)
import AST (AST(..))
import Utils (Safe(..))
import Data.Bits
import Data.Char(ord, chr)

binaryBuiltins :: Symbols
binaryBuiltins = [ BackendSymbol ("&", astBinaryOp "&" (.&.))
            ,   BackendSymbol ("band", astBinaryOp "band" (.&.))
            ,   BackendSymbol ("|", astBinaryOp "|" (.|.))
            ,   BackendSymbol ("bor", astBinaryOp "bor" (.|.))
            ,   BackendSymbol ("~", astBNot)
            ,   BackendSymbol ("bnot", astBNot)
            ,   BackendSymbol ("^", astBinaryOp "^" xor)
            ,   BackendSymbol ("bxor", astBinaryOp "bxor" xor)
            ,   BackendSymbol (">>", astBinaryOp ">>" (.>>.))
            ,   BackendSymbol ("rshift", astBinaryOp ">>" (.>>.))
            ,   BackendSymbol ("<<", astBinaryOp "<<" (.<<.))
            ,   BackendSymbol ("lshift", astBinaryOp "<<" (.<<.))]


astBinaryOp' :: String -> (Int -> Int -> Int) -> [AST] -> Safe AST
astBinaryOp' _ f [ASTInt a, ASTInt b] = Value $ ASTInt $ f a b
astBinaryOp' _ f [ASTInt a, ASTUInt b] = Value $ ASTInt $ f a b
astBinaryOp' _ f [ASTInt a, ASTChar b] = Value $ ASTInt $ f a (ord b)
astBinaryOp' _ f [ASTUInt a, ASTUInt b] = Value $ ASTUInt $ f a b
astBinaryOp' _ f [ASTChar a, ASTUInt b] = Value $ ASTUInt $ f (ord a) b
astBinaryOp' _ f [ASTChar a, ASTChar b] = Value $ ASTChar $ chr $ f (ord a) (ord b)
astBinaryOp' name _ args = Error ("Bad arguments when attempting to call " ++ name ++ " ! Expected 2 integers but got " ++ show args ++ " !")

astBinaryOp :: String -> (Int -> Int -> Int) -> Symbols -> [AST] -> Safe AST
astBinaryOp name f symbols args = mapM (fst . evaluateAST1 symbols) args >>= astBinaryOp' name f

astBNot' :: [AST] -> Safe AST
astBNot' [ASTInt a] = Value $ ASTInt $ complement a
astBNot' [ASTUInt a] = Value $ ASTUInt $ complement a
astBNot' [ASTChar a] = Value $ ASTChar $ chr $ complement (ord a)
astBNot' args = Error ("Bad arguments when attempting to call '~' ! Expected an integer but got " ++ show args ++ " !")

astBNot :: Symbols -> [AST] -> Safe AST
astBNot symbols args = mapM (fst . evaluateAST1 symbols) args >>= astBNot'