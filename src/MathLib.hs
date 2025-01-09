module MathLib (mathBuiltins) where
import Evaluate(evaluateAST1, Symbol(BackendSymbol), Symbols)
import AST (AST(..))
import Utils (Safe(..))
import Data.Char(ord, chr)

factorial :: Int -> Int
factorial n = product [1..n] 

mathBuiltins :: Symbols
mathBuiltins = [    BackendSymbol ("*", astArithmeticOp "*" (*))
            ,   BackendSymbol ("+", astArithmeticOp "+" (+))
            ,   BackendSymbol ("-", astArithmeticOp "-" (-))
            ,   BackendSymbol ("div", astArithmeticOp "div" (/))
            ,   BackendSymbol ("mod", astModulo)
            ,   BackendSymbol ("**", astArithmeticOp "**" (**))
            ,   BackendSymbol ("v-", astArithmeticSingleOp "v-" sqrt)
            ,   BackendSymbol ("!!", astFactorial)
            ,   BackendSymbol ("pi", \_ _ -> Value (ASTFloat 3.14159265))
            ,   BackendSymbol ("e", \_ _ -> Value (ASTFloat 2.71828182)) -- verifié liste parametre
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
astArithmeticOp' _ f [ASTFloat a, ASTFloat b] = Value $ ASTFloat (f a b)
astArithmeticOp' _ f [ASTInt a, ASTFloat b] = Value $ ASTFloat (f (fromIntegral a) b)
astArithmeticOp' _ f [ASTUInt a, ASTFloat b] = Value $ ASTFloat (f (fromIntegral a) b)
astArithmeticOp' _ f [ASTChar a, ASTFloat b] = Value $ ASTFloat (f (fromIntegral (ord a)) b)
astArithmeticOp' _ f [ASTInt a, ASTInt b] = Value $ ASTInt $ floor (f (fromIntegral a) (fromIntegral b))
astArithmeticOp' _ f [ASTInt a, ASTUInt b] = Value $ ASTInt $ floor (f (fromIntegral a) (fromIntegral b))
astArithmeticOp' _ f [ASTInt a, ASTChar b] = Value $ ASTInt $ floor (f (fromIntegral a) (fromIntegral (ord b)))
astArithmeticOp' _ f [ASTUInt a, ASTUInt b] = Value $ ASTUInt $ floor (f (fromIntegral a) (fromIntegral b))
astArithmeticOp' _ f [ASTChar a, ASTUInt b] = Value $ ASTUInt $ floor (f (fromIntegral (ord a)) (fromIntegral b))
astArithmeticOp' _ f [ASTChar a, ASTChar b] = Value $ ASTChar $ chr $ floor (f (fromIntegral (ord a)) (fromIntegral (ord b)))
astArithmeticOp' name _ args = Error ("Bad arguments when attempting to call " ++ name ++ " ! Expected 2 numbers but got " ++ show args ++ " !")

astArithmeticOp :: String -> (Float -> Float -> Float) -> Symbols -> [AST] -> Safe AST
astArithmeticOp name f symbols args = mapM (fst . evaluateAST1 symbols) args >>= astArithmeticOp' name f

astModulo' :: Symbols -> [AST] -> Safe AST
astModulo' _ [ASTInt a, ASTInt b] = Value $ ASTInt (mod a b)
astModulo' _ [ASTInt a, ASTUInt b] = Value $ ASTInt (mod a b)
astModulo' _ [ASTInt a, ASTChar b] = Value $ ASTInt (mod a (ord b))
astModulo' _ [ASTUInt a, ASTUInt b] = Value $ ASTUInt (mod a b)
astModulo' _ [ASTUInt a, ASTChar b] = Value $ ASTUInt (mod a (ord b))
astModulo' _ [ASTChar a, ASTChar b] = Value $ ASTChar $ chr $ mod (ord a) (ord b)
astModulo' _ args = Error ("Bad arguments when attempting to call '%'! expected an integer but got " ++ show args ++ " !")

astModulo :: Symbols -> [AST] -> Safe AST
astModulo symbols arg =  mapM (fst . evaluateAST1 symbols) arg >>= astModulo' symbols

astFactorial' :: Symbols -> [AST] -> Safe AST
astFactorial' _ [ASTInt a]
                        | a >= 0 = Value $ ASTUInt (factorial a)
                        | otherwise = Error ("Bad arguments when attempting to call '!'! expected a positive integer but got " ++ show a ++ " !")
astFactorial' _ [ASTUInt a] = Value $ ASTUInt (factorial a)
astFactorial' _ [ASTChar a] = Value $ ASTUInt (factorial (ord a))
astFactorial' _ args = Error ("Bad arguments when attempting to call '!!'! expected an integer but got " ++ show args ++ " !")

astFactorial :: Symbols -> [AST] -> Safe AST
astFactorial symbols arg = mapM (fst . evaluateAST1 symbols) arg >>= astFactorial' symbols

astArithmeticSingleOp' :: String -> (Float -> Float) -> [AST] -> Safe AST
astArithmeticSingleOp' _ f [ASTFloat a] = Value $ ASTFloat (f a)
astArithmeticSingleOp' _ f [ASTInt a] = Value $ ASTFloat $ f (fromIntegral a)
astArithmeticSingleOp' _ f [ASTUInt a] = Value $ ASTFloat $ f (fromIntegral a)
astArithmeticSingleOp' _ f [ASTChar a] = Value $ ASTFloat $ f (fromIntegral (ord a))
astArithmeticSingleOp' name _ args = Error ("Bad arguments when attempting to call " ++ name ++ "! expected a number but got " ++ show args ++ " !")

astArithmeticSingleOp :: String -> (Float -> Float) -> Symbols -> [AST] -> Safe AST
astArithmeticSingleOp name f symbols args = mapM (fst . evaluateAST1 symbols) args >>= astArithmeticSingleOp' name f

astRounding' :: String -> (Float -> Int) -> [AST] -> Safe AST
astRounding' _ f [ASTFloat a] = Value $ ASTFloat $ fromIntegral (f a)
astRounding' name _ args = Error ("Bad arguments when attempting to call " ++ name ++ "! expected a float but got " ++ show args ++ " !")

astRounding :: String -> (Float -> Int) -> Symbols -> [AST] -> Safe AST
astRounding name f symbols arg = mapM (fst . evaluateAST1 symbols) arg >>= astRounding' name f
