module Evaluate (evaluateAST, Symbol(..), Symbols) where

import AST (AST(..), Call(..), MainAST(..))
import Data.List (singleton)
import Utils (Safe(..))
import Type

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f (x : xs) = if (f x) then Just x else find f xs

data Symbol = BackendSymbol (String, (Symbols -> [AST] -> Safe AST))
type Symbols = [Symbol]

symbolName :: Symbol -> String
symbolName (BackendSymbol (s, _)) = s

showSymbols :: Symbols -> String
showSymbols symbols = show (map symbolName symbols)

-- traceSymbols2 :: String -> Symbols -> Symbols
-- traceSymbols2 msg f = debug2 msg (map symbolName f) f

-- traceSymbols :: Symbols -> Symbols
-- traceSymbols = traceSymbols2 "symbols: "

-- traceSymbol :: Maybe Symbol -> Maybe Symbol
-- traceSymbol f@(Just (BackendSymbol (s, _))) = debug2 "symbol: " s f
-- traceSymbol Nothing = Nothing

builtins :: Symbols
builtins = [    BackendSymbol ("*", astArithmeticOp "*" (*))
            ,   BackendSymbol ("+", astArithmeticOp "+" (+))
            ,   BackendSymbol ("-", astArithmeticOp "-" (-))
            ,   BackendSymbol ("div", astArithmeticOp "div" div)
            ,   BackendSymbol ("mod", astArithmeticOp "mod" mod)
            ,   BackendSymbol (">", astComparisonOp ">" (>))
            ,   BackendSymbol (">=", astComparisonOp ">=" (>=))
            ,   BackendSymbol ("<", astComparisonOp "<" (<))
            ,   BackendSymbol ("<=", astComparisonOp "<=" (<=))
            ,   BackendSymbol ("eq?", astComparisonOp "eq?" (==))
            ,   BackendSymbol ("==", astComparisonOp "==" (==))
            ,   BackendSymbol ("if", astIf)]

updateOrAdd :: (a -> Bool) -> a -> [a] -> [a]
updateOrAdd _ a [] = [a]
updateOrAdd f a (x : xs)
    | f x = a : xs
    | otherwise = x : updateOrAdd f a xs

registerSymbol :: Symbols -> String -> (Symbols -> [AST] -> Safe AST) -> Symbols
registerSymbol symbols name f = updateOrAdd ((== name) . symbolName) (BackendSymbol (name, f)) symbols

symbolId :: String -> AST -> Symbols -> [AST] -> Safe AST
symbolId _ value _ [] = Value value
symbolId name _ _ args = Error ("Value " ++ name ++ " must be called without any argument, but you provided " ++ show args ++ " !")

fromSymbol :: AST -> Safe String
fromSymbol (ASTProcedure s) = Value s
fromSymbol arg = Error ("Lambda arguments list must only contain symbols, but got " ++ show arg ++ " !")

updateSymbols :: Symbols -> [(String, AST)] -> Symbols
updateSymbols symbols new = foldr (\(symbol, value) symbols' -> registerSymbol symbols' symbol (symbolId symbol value)) symbols new

makeLambdaSymbols :: Symbols -> [AST] -> [AST] -> Safe Symbols
makeLambdaSymbols symbols [] _ = Value symbols
makeLambdaSymbols symbols _ [] = Value symbols
makeLambdaSymbols symbols lambdaParams lambdaArgs = fmap (\params' -> updateSymbols symbols (zip params' lambdaArgs)) (mapM fromSymbol lambdaParams)

toSafe :: String -> Maybe a -> Safe a
toSafe err Nothing = Error err
toSafe _ (Just a) = Value a

findSymbol :: Symbols -> String -> Safe Symbol
findSymbol symbols symbol = toSafe ("*** ERROR : variable " ++ symbol ++ " is not bound.") (find ((== symbol) . symbolName) symbols)

evaluateAST1 :: Symbols -> AST -> (Safe AST, Symbols)
evaluateAST1 symbols n@(ASTInt _) = (Value n, symbols)
evaluateAST1 symbols b@(ASTBool _) = (Value b, symbols)
evaluateAST1 symbols lambda@(ASTLambda _ _ _) = (Value lambda, symbols)
evaluateAST1 symbols (ASTProcedure s) = (findSymbol symbols s >>= (\(BackendSymbol (_, f)) -> f symbols [] >>= (fst . evaluateAST1 symbols)), symbols)
evaluateAST1 symbols (ASTCall (FunctionCall f) args) = (findSymbol symbols f >>= (\(BackendSymbol (_, f')) -> f' symbols args), symbols)
evaluateAST1 symbols (ASTCall (LambdaCall params body _) args) = (args' >>= (makeLambdaSymbols symbols (map fst params)) >>= (\symbols' -> fst $ evaluateAST1 symbols' body), symbols) -- discarding lambda symbols and returning old ones
    where args' = if null args then Value [] else (mapM (fst . evaluateAST1 symbols) args)
evaluateAST1 symbols define@(ASTDefine s _ (ASTLambda params body _)) = (Value define, registerSymbol symbols s function)
    where function symbols' args = mapM (fst . evaluateAST1 symbols') args >>= (\args' -> fst $ evaluateAST1 symbols' (ASTCall (LambdaCall params body T_Undefined) args'))
evaluateAST1 symbols define@(ASTDefine s _ ast) = (Value define, registerSymbol symbols s function)
    where function symbols' args
            | null args = Value ast
            | length args >= 2 = Error ("Too many arguments when calling symbol ! Got " ++ show (length args) ++ " but expected 0 or 1 !")
            | otherwise = fst $ evaluateAST1 symbols' $ head args

astArithmeticOp' :: String -> (Int -> Int -> Int) -> [AST] -> Safe AST
astArithmeticOp' _ f [(ASTInt a), (ASTInt b)] = Value $ ASTInt (f a b)
astArithmeticOp' name _ args = Error ("Bad arguments when attempting to call " ++ name ++ " ! Expected 2 integers but got " ++ show args ++ " !")

astArithmeticOp :: String -> (Int -> Int -> Int) -> Symbols -> [AST] -> Safe AST
astArithmeticOp name f symbols args = mapM (fst . evaluateAST1 symbols) args >>= astArithmeticOp' name f

toNumber :: AST -> Safe Int
toNumber (ASTBool b) = Value (fromEnum b)
toNumber (ASTInt n) = Value n
toNumber a = Error ("Cannot convert " ++ show a ++ " to an integer !")

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

evaluateAST' :: Symbols -> [AST] -> Safe [MainAST]
evaluateAST' _ [] = Error "Nothing to evaluate !"
evaluateAST' f [x] = fmap (singleton . MainAST) (fst $ evaluateAST1 f x)
evaluateAST' f (x : xs) = liftA2 (:) (fmap MainAST evaluated) rest
    where (evaluated, symbols) = evaluateAST1 f x
          rest = evaluateAST' symbols xs

evaluateAST :: [AST] -> Safe [MainAST]
evaluateAST = evaluateAST' builtins

-- test :: [SExpr]
-- test = [ List [Symbol "define", Symbol "x", List [Symbol "+", Number 6, Number 5]]
        -- , List [Symbol "eq?", Number 1, List [Symbol "mod", List [Symbol "div", List [Symbol "*", Number 5, List [Symbol "+", Number 7, List [Symbol "-", Number 10, Number 2]]], Number 5], Number 7]]
        -- , List [Symbol "if", List [Symbol ">", Symbol "x", Number 8], Symbol "#t", Symbol "#f"] ]
-- test = [ List [Symbol "define", Symbol "a", Number 5], List [List [Lambda (List [Symbol "a", Symbol "b"]) (List [Symbol "+", Symbol "a", Symbol "b"])], List [Symbol "+", Symbol "a", Symbol "a"], Symbol "a"]]
-- test = [List [Symbol "define", Symbol "x", Li /home/Chi_Iroh/.gst [Lambda (List [Symbol "a", Symbol "b"]) (List [Symbol "+", Symbol "a", Symbol "b"])]], List [Symbol "x", Number 5, Number 10]]
-- test = [List [List [Lambda (List []) (Number 15)]]]
-- test = [List [List [Lambda (List [Symbol "a"]) (List [Symbol "if", List [Symbol "eq?", Symbol "a", Number 1], Symbol "#t", Symbol "#f"])], Number 1]]
-- test = [ List [Symbol "define", Symbol "factorial", List [Lambda (List [Symbol "n"]) (List [Symbol "if", List [Symbol "<=", Symbol "n", Number 1], Number 1, List [Symbol "*", Symbol "n", List [Symbol "factorial", List [Symbol "-", Symbol "n", Number 1]]]])]], List [Symbol "factorial", Number 10]]

-- putMaybeStr :: Maybe String -> IO ()
-- putMaybeStr str = putStr $ fromMaybe "Nothing" str

-- putMaybeStrLn :: Maybe String -> IO ()
-- putMaybeStrLn str = putStrLn $ fromMaybe "Nothing" str

-- main :: IO ()
-- main = putStr $ join "\n" $ map (show . sexprToAST) test
-- main = putMaybeStrLn (fmap (\x -> join "\n" (map ((++) "Result: " . show) x)) (mapM sexprToAST (debug "SExpr: " test) >>= evaluateAST . debug "AST: "))
