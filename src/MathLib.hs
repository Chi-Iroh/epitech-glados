module MathLib (mathBuiltins) where
import Utils (Safe(..))
import Data.Char(ord, chr)
import VMData(Any(..))
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
            ,   BackendBuiltins ("pi", \_ -> Value (Float 3.14159265))
            ,   BackendBuiltins ("e", \_ -> Value (Float 2.71828182)) -- verifié liste parametre
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

pdpModulo :: [Any] -> Safe Any
pdpModulo [Int a, Int b] = Value $ Int (mod a b)
pdpModulo [Int a, UInt b] = Value $ Int (mod a b)
pdpModulo [Int a, Char b] = Value $ Int (mod a (ord b))
pdpModulo [UInt a, UInt b] = Value $ UInt (mod a b)
pdpModulo [UInt a, Char b] = Value $ UInt (mod a (ord b))
pdpModulo [Char a, Char b] = Value $ Char $ chr $ mod (ord a) (ord b)
pdpModulo args = Error ("Bad arguments when attempting to call '%'! expected an integer but got " ++ show args ++ " !")

pdpFactorial :: [Any] -> Safe Any
pdpFactorial [Int a]
                        | a >= 0 = Value $ UInt (factorial a)
                        | otherwise = Error ("Bad arguments when attempting to call '!'! expected a positive integer but got " ++ show a ++ " !")
pdpFactorial [UInt a] = Value $ UInt (factorial a)
pdpFactorial [Char a] = Value $ UInt (factorial (ord a))
pdpFactorial args = Error ("Bad arguments when attempting to call '!!'! expected an integer but got " ++ show args ++ " !")


pdpArithmeticSingleOp :: String -> (Float -> Float) -> [Any] -> Safe Any
pdpArithmeticSingleOp _ f [Float a] = Value $ Float (f a)
pdpArithmeticSingleOp _ f [Int a] = Value $ Float $ f (fromIntegral a)
pdpArithmeticSingleOp _ f [UInt a] = Value $ Float $ f (fromIntegral a)
pdpArithmeticSingleOp _ f [Char a] = Value $ Float $ f (fromIntegral (ord a))
pdpArithmeticSingleOp name _ args = Error ("Bad arguments when attempting to call " ++ name ++ "! expected a number but got " ++ show args ++ " !")

pdpRounding :: String -> (Float -> Int) -> [Any] -> Safe Any
pdpRounding _ f [Float a] = Value $ Float $ fromIntegral (f a)
pdpRounding name _ args = Error ("Bad arguments when attempting to call " ++ name ++ "! expected a float but got " ++ show args ++ " !")
