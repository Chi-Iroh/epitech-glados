module MathLib (mathBuiltins) where
import Utils (Safe(..))
import Data.Char(ord, chr)
import VMData(Any)
import DataBuiltins (Symbols, BuiltinsSymbol(BackendBuiltins))

factorial :: Int -> Int
factorial n = product [1..n] 

mathBuiltins :: Symbols
mathBuiltins = [    BackendBuiltins ("*", pdpArithmeticOp "*" (*))
            ,       BackendBuiltins ("mul", pdpArithmeticOp "mul" (*))
            ,   BackendBuiltins ("+", pdpArithmeticOp "+" (+))
            ,   BackendBuiltins ("add", pdpArithmeticOp "add" (+))
            ,   BackendBuiltins ("-", pdpArithmeticOp "-" (-))
            ,   BackendBuiltins ("sub", pdpArithmeticOp "sub" (-))
            ,   BackendBuiltins ("div", pdpArithmeticOp "div" (/))
            ,   BackendBuiltins ("/", pdpArithmeticOp "div" (/))
            ,   BackendBuiltins ("mod", pdpModulo)
            ,   BackendBuiltins ("%", pdpModulo)
            ,   BackendBuiltins ("**", pdpArithmeticOp "**" (**))
            ,   BackendBuiltins ("pow", pdpArithmeticOp "pow" (**))
            ,   BackendBuiltins ("v-", pdpArithmeticSingleOp "v-" sqrt)
            ,   BackendBuiltins ("sqrt", pdpArithmeticSingleOp "v-" sqrt)
            ,   BackendBuiltins ("!!", pdpFactorial)
            ,   BackendBuiltins ("factorial", pdpFactorial)
            ,   BackendBuiltins ("pi", \_ _ -> Value (Float 3.14159265))
            ,   BackendBuiltins ("e", \_ _ -> Value (Float 2.71828182)) -- verifié liste parametre
            ,   BackendBuiltins ("exp", pdpArithmeticSingleOp "exp" exp)
            ,   BackendBuiltins ("ln", pdpArithmeticSingleOp "ln" log)
            ,   BackendBuiltins ("max", pdpArithmeticOp "max" max)
            ,   BackendBuiltins ("min", pdpArithmeticOp "min" min)
            ,   BackendBuiltins ("cos", pdpArithmeticSingleOp "cos" cos)
            ,   BackendBuiltins ("acos", pdpArithmeticSingleOp "acos" acos)
            ,   BackendBuiltins ("cosh", pdpArithmeticSingleOp "cosh" cosh)
            ,   BackendBuiltins ("sin", pdpArithmeticSingleOp "sin" sin)
            ,   BackendBuiltins ("asin", pdpArithmeticSingleOp "asin" asin)
            ,   BackendBuiltins ("sinh", pdpArithmeticSingleOp "sinh" sinh)
            ,   BackendBuiltins ("tan", pdpArithmeticSingleOp "tan" tan)
            ,   BackendBuiltins ("atan", pdpArithmeticSingleOp "atan" atan)
            ,   BackendBuiltins ("ceil", pdpRounding "ceil" ceiling)
            ,   BackendBuiltins ("round", pdpRounding "round" round)
            ,   BackendBuiltins ("trunc", pdpRounding "trunc" truncate)
            ,   BackendBuiltins ("floor", pdpRounding "floor" floor)]

pdpArithmeticOp :: String -> (Float -> Float -> Float) -> [Any] -> Safe Any
pdpArithmeticOp _ f [Float a, Float b] = Value $ Float (f a b)
pdpArithmeticOp _ f [Int a, Float b] = Value $ Float (f (fromIntegral a) b)
pdpArithmeticOp _ f [UInt a, Float b] = Value $ Float (f (fromIntegral a) b)
pdpArithmeticOp _ f [Char a, Float b] = Value $ Float (f (fromIntegral (ord a)) b)
pdpArithmeticOp _ f [Int a, Int b] = Value $ Int $ floor (f (fromIntegral a) (fromIntegral b))
pdpArithmeticOp _ f [Int a, UInt b] = Value $ Int $ floor (f (fromIntegral a) (fromIntegral b))
pdpArithmeticOp _ f [Int a, Char b] = Value $ Int $ floor (f (fromIntegral a) (fromIntegral (ord b)))
pdpArithmeticOp _ f [UInt a, UInt b] = Value $ UInt $ floor (f (fromIntegral a) (fromIntegral b))
pdpArithmeticOp _ f [Char a, UInt b] = Value $ UInt $ floor (f (fromIntegral (ord a)) (fromIntegral b))
pdpArithmeticOp _ f [Char a, Char b] = Value $ Char $ chr $ floor (f (fromIntegral (ord a)) (fromIntegral (ord b)))
pdpArithmeticOp name _ args = Error ("Bad arguments when attempting to call " ++ name ++ " ! Expected 2 numbers but got " ++ show args ++ " !")

pdpModulo' :: Symbols -> [Any] -> Safe Any
pdpModulo' _ [Int a, Int b] = Value $ Int (mod a b)
pdpModulo' _ [Int a, UInt b] = Value $ Int (mod a b)
pdpModulo' _ [Int a, Char b] = Value $ Int (mod a (ord b))
pdpModulo' _ [UInt a, UInt b] = Value $ UInt (mod a b)
pdpModulo' _ [UInt a, Char b] = Value $ UInt (mod a (ord b))
pdpModulo' _ [Char a, Char b] = Value $ Char $ chr $ mod (ord a) (ord b)
pdpModulo' _ args = Error ("Bad arguments when attempting to call '%'! expected an integer but got " ++ show args ++ " !")

pdpModulo :: Symbols -> [Any] -> Safe Any
pdpModulo symbols arg =  mapM (fst . evaluateAST1 symbols) arg >>= pdpModulo' symbols

pdpFactorial :: Symbols -> [Any] -> Safe Any
pdpFactorial _ [Int a]
                        | a >= 0 = Value $ UInt (factorial a)
                        | otherwise = Error ("Bad arguments when attempting to call '!'! expected a positive integer but got " ++ show a ++ " !")
pdpFactorial _ [UInt a] = Value $ UInt (factorial a)
pdpFactorial _ [Char a] = Value $ UInt (factorial (ord a))
pdpFactorial _ args = Error ("Bad arguments when attempting to call '!!'! expected an integer but got " ++ show args ++ " !")


pdpArithmeticSingleOp' :: String -> (Float -> Float) -> [Any] -> Safe Any
pdpArithmeticSingleOp' _ f [Float a] = Value $ Float (f a)
pdpArithmeticSingleOp' _ f [Int a] = Value $ Float $ f (fromIntegral a)
pdpArithmeticSingleOp' _ f [UInt a] = Value $ Float $ f (fromIntegral a)
pdpArithmeticSingleOp' _ f [Char a] = Value $ Float $ f (fromIntegral (ord a))
pdpArithmeticSingleOp' name _ args = Error ("Bad arguments when attempting to call " ++ name ++ "! expected a number but got " ++ show args ++ " !")

pdpArithmeticSingleOp :: String -> (Float -> Float) -> Symbols -> [Any] -> Safe Any
pdpArithmeticSingleOp name f symbols args = mapM (fst . evaluateAST1 symbols) args >>= pdpArithmeticSingleOp' name f

pdpRounding' :: String -> (Float -> Int) -> [Any] -> Safe Any
pdpRounding' _ f [Float a] = Value $ Float $ fromIntegral (f a)
pdpRounding' name _ args = Error ("Bad arguments when attempting to call " ++ name ++ "! expected a float but got " ++ show args ++ " !")

pdpRounding :: String -> (Float -> Int) -> Symbols -> [Any] -> Safe Any
pdpRounding name f symbols arg = mapM (fst . evaluateAST1 symbols) arg >>= pdpRounding' name f
