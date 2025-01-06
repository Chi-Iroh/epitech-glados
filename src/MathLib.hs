module MathLib (mathBuiltins) where
import Evaluate(evaluateAST1, Symbol, Symbols)
import AST (AST(..), Call(..), MainAST(..))
import Data.List (singleton)
import Utils (Safe(..))
import Type

factorial :: Int -> Int
factorial n = product [1..n] 

mathBuiltins :: Symbols
mathBuiltins = [    BackendSymbol ("*", astArithmeticOp "*" (*))
            ,   BackendSymbol ("+", astArithmeticOp "+" (+))
            ,   BackendSymbol ("-", astArithmeticOp "-" (-))
            ,   BackendSymbol ("div", astArithmeticOp "div" div)
            ,   BackendSymbol ("mod", astArithmeticOp "mod" mod)
            ,   BackendSymbol ("**", astArithmeticOp "**" (**))
            ,   BackendSymbol ("v-", astArithmeticSingleOp "v-" sqrt)
            ,   BackendSymbol ("!!", astFactorial)
            ,   BackendSymbol ("pi", 3.14159265)
            ,   BackendSymbol ("e", 2.71828182)
            ,   BackendSymbol ("exp", astArithmeticSingleOp "exp" exp)
            ,   BackendSymbol ("ln", astArithmeticSingleOp "ln" log)
            ,   BackendSymbol ("max", astArithmeticOp "max" max)
            ,   BackendSymbol ("min", astArithmeticOp "min" min)
            ,   BackendSymbol ("cos", astArithmeticSingleOp "cos" cos)
            ,   BackendSymbol ("acos", astArithmeticSingleOp "acos" acos)
            ,   BackendSymbol ("cosh", astArithmeticSingleOp "cosh" cosh)
            ,   BackendSymbol ("sin", astArithmeticSingleOp "sin" sin)
            ,   BackendSymbol ("asin", astArithmeticSingleOp "asin" asin)
            ,   BackendSymbol ("sinh", astArithmeticSingleOp "sinh" sinh)
            ,   BackendSymbol ("tan", astArithmeticSingleOp "tan" tan)
            ,   BackendSymbol ("atan", astArithmeticSingleOp "atan" atan)
            ,   BackendSymbol ("ceil", astRounding "ceil" ceiling)
            ,   BackendSymbol ("round", astRounding "round" round)
            ,   BackendSymbol ("trunc", astRounding "trunc" truncate)
            ,   BackendSymbol ("floor", astRounding "floor" floor)]

astArithmeticOp' :: String -> (Float -> Float -> Float) -> [AST] -> Safe AST
astArithmeticOp' _ f [(ASTFloat a), (_ b)] = Value $ ASTFloat (f a b)
astArithmeticOp' _ f [(_ a), (ASTFloat b)] = Value $ ASTFloat (f a b)
astArithmeticOp' _ f [(_ a), (ASTInt b)] = Value $ ASTInt (f a b)
astArithmeticOp' _ f [(ASTInt a), (_ b)] = Value $ ASTInt (f a b)
astArithmeticOp' _ f [(ASTUInt a), (_ b)] = Value $ ASTUInt (f a b)
astArithmeticOp' _ f [(_ a), (ASTUInt b)] = Value $ ASTUInt (f a b)
astArithmeticOp' _ f [(ASTChar a), (ASTChar b)] = Value $ ASTChar (f a b)
astArithmeticOp' name _ args = Error ("Bad arguments when attempting to call " ++ name ++ " ! Expected 2 numbers but got " ++ show args ++ " !")

astArithmeticOp :: String -> (Float -> Float -> Float) -> Symbols -> [AST] -> Safe AST
astArithmeticOp name f symbols args = mapM (fst . evaluateAST1 symbols) args >>= astArithmeticOp' name f

astFactorial' :: Symbols -> AST -> Safe AST
astFactorial' _ (ASTInt a)
                        | a >= 0 = Value $ ASTUInt (factorial a)
                        | otherwise = Error ("Bad arguments when attempting to call '!'! expected a positive integer but got " ++ show a ++ " !")
astFactorial' _ (ASTUInt a) = Value $ ASTUInt (factorial a)
astFactorial' _ (ASTChar a) = Value $ ASTUInt (factorial a)
astFactorial' _ args = Error ("Bad arguments when attempting to call '!'! expected an integer but got " ++ show args ++ " !")

astFactorial :: Symbols -> AST -> Safe AST
astFactorial symbols arg = astRoot' $ fst . evaluateAST1 symbols args

astArithmeticSingleOp' :: String -> (Float -> Float) -> AST -> Safe AST
astArithmeticSingleOp' _ f (ASTFloat a) = Value $ ASTFloat (f a)
astArithmeticSingleOp' _ f (ASTInt a) = Value $ ASTFloat (f a)
astArithmeticSingleOp' _ f (ASTUInt a) = Value $ ASTFloat (f a)
astArithmeticSingleOp' _ f (ASTChar a) = Value $ ASTFloat (f a)
astArithmeticSingleOp' name _ args = Error ("Bad arguments when attempting to call " ++ name ++ "! expected a number but got " ++ show args ++ " !")

astArithmeticSingleOp :: String -> (Float -> Float) -> Symbols -> AST -> Safe AST
astArithmeticSingleOp name f symbols arg = astArithmeticSingleOp' name f (fst . evaluateAST1 symbols args)

astRounding' :: String -> (Float -> Float) -> AST -> Safe AST
astRounding' _ f (ASTFloat a) = Value $ ASTFloat (f a)
astRounding' name _ args = Error ("Bad arguments when attempting to call " ++ name ++ "! expected a float but got " ++ show args ++ " !")

astRounding :: String -> (Float -> Float) -> Symbols -> AST -> Safe AST
astRounding name f symbols arg = astArithmeticSingleOp' name f (fst . evaluateAST1 symbols args)
