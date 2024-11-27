module Main where

import Control.Applicative (liftA3)
import Data.List (singleton)
import Data.Maybe (isJust, fromMaybe)
import Debug.Trace (traceShowId, trace)

-- debug "Hello " 4 prints "Hello 4" and returns 4
debug :: Show a => String -> a -> a
debug msg a = trace (msg ++ (show a)) a

-- debug "Hello " 4 5 prints "Hello 4" and returns 5
debug2 :: Show a => String -> a -> b -> b
debug2 msg a b = trace (msg ++ (show a)) b

data SExpr = Number Int | Symbol String | List [SExpr] | Define String SExpr | Lambda SExpr SExpr deriving Show
data Call = LambdaCall [AST] AST | FunctionCall String deriving Show
data AST = ASTDefine String AST | ASTSymbol String | ASTNumber Int | ASTBoolean Bool | ASTCall Call [AST] | ASTLambda [AST] AST deriving Show

sexprToAST :: SExpr -> Maybe AST
sexprToAST (Number n) = Just $ ASTNumber n
sexprToAST (Symbol "#t") = Just $ ASTBoolean True
sexprToAST (Symbol "#f") = Just $ ASTBoolean False
sexprToAST (Symbol "define") = Nothing
sexprToAST (Symbol x) = Just $ ASTSymbol x
sexprToAST (Lambda _ _) = Nothing
sexprToAST (List [(Symbol "define")]) = Nothing
sexprToAST (List [(Symbol "define"), (Symbol x), expr]) = sexprToAST expr >>= (\expr -> Just $ ASTDefine x expr)
sexprToAST (List ((Symbol "define") : xs)) = Nothing
sexprToAST (List [(Lambda (List params) body)]) = liftA2 (\params' body' -> ASTLambda params' body') (mapM sexprToAST params) (sexprToAST body)
sexprToAST (List (List [Lambda (List params) body] : args)) = liftA3 (\params' body' args' -> ASTCall (LambdaCall params' body') args') (mapM sexprToAST params) (sexprToAST body) (mapM sexprToAST args)
sexprToAST (List ((Symbol x) : xs)) = mapM sexprToAST xs >>= (\xs -> Just $ ASTCall (FunctionCall x) xs)
sexprToAST _ = Nothing

builtins :: Symbols
builtins = [("*", astArithmeticOp (*))
            , ("+", astArithmeticOp (+))
            , ("-", astArithmeticOp (-))
            , ("div", astArithmeticOp div)
            , ("mod", astArithmeticOp mod)
            , (">", astComparisonOp (>))
            , ("<", astComparisonOp (<))
            , ("eq?", astComparisonOp (==))
            , ("if", astIf)]

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f (x : xs) = if (f x) then Just x else find f xs

isBuiltin :: String -> Bool
isBuiltin x = isJust $ find ((== x) . fst) builtins

type Symbol = (String, ([AST] -> Maybe AST))
type Symbols = [Symbol]

traceSymbols :: Symbols -> Symbols
traceSymbols f = (debug "symbols " $ map fst f) >> f

traceSymbol :: Maybe Symbol -> Maybe Symbol
traceSymbol f@(Just _) = f
traceSymbol Nothing = Nothing

updateOrAdd :: (a -> Bool) -> a -> [a] -> [a]
updateOrAdd _ a [] = [a]
updateOrAdd f a (x : xs)
    | f x = a : xs
    | otherwise = x : updateOrAdd f a xs

registerSymbol :: Symbols -> String -> ([AST] -> Maybe AST) -> Symbols
registerSymbol symbols name f = updateOrAdd ((== name) . fst) (name, f) symbols

symbolId :: AST -> [AST] -> Maybe AST
symbolId value [] = Just value
symbolId _ _ = Nothing

fromSymbol :: AST -> Maybe String
fromSymbol (ASTSymbol s) = Just s
fromSymbol _ = Nothing

updateSymbols :: Symbols -> [(String, AST)] -> Symbols
updateSymbols symbols new = foldr (\(symbol, value) symbols' -> registerSymbol symbols' symbol (symbolId value)) symbols new

makeLambdaSymbols :: Symbols -> [AST] -> [AST] -> Maybe Symbols
makeLambdaSymbols symbols [] _ = Just symbols
makeLambdaSymbols symbols _ [] = Just symbols
makeLambdaSymbols symbols lambdaParams lambdaArgs = fmap (\params' -> updateSymbols symbols (zip params' lambdaArgs)) (mapM fromSymbol lambdaParams)

evaluateAST1 :: Symbols -> AST -> (Maybe AST, Symbols)
evaluateAST1 symbols n@(ASTNumber _) = (Just n, symbols)
evaluateAST1 symbols b@(ASTBoolean _) = (Just b, symbols)
evaluateAST1 symbols lambda@(ASTLambda _ _) = (Just lambda, symbols)
evaluateAST1 symbols (ASTSymbol s) = (find ((== s) . fst) symbols >>= (\(_, f) -> f [] >>= (fst . evaluateAST1 symbols)), symbols)
evaluateAST1 symbols (ASTCall (FunctionCall f) args) = ((find ((== f) . fst) symbols) >>= (\(s, f) -> evaluateAST' symbols args >>= f), symbols)
evaluateAST1 symbols (ASTCall (LambdaCall params body) args) = (args' >>= (makeLambdaSymbols symbols params) >>= (\symbols' -> fst $ evaluateAST1 symbols' body), symbols) -- discarding lambda symbols and returning old ones
    where args' = if null args then Just [] else (mapM (fst . evaluateAST1 symbols) args)
evaluateAST1 symbols define@(ASTDefine s lambda@(ASTLambda params body)) = (Just define, registerSymbol symbols s function)
    where function args = mapM (fst . evaluateAST1 symbols) args >>= (\args' -> fst $ evaluateAST1 symbols (ASTCall (LambdaCall params body) args'))
evaluateAST1 symbols define@(ASTDefine s ast) = (Just define, registerSymbol symbols s function)
    where function args
            | null args = Just ast
            | length args >= 2 = Nothing
            | otherwise = fst $ evaluateAST1 symbols $ head args

astArithmeticOp :: (Int -> Int -> Int) -> [AST] -> Maybe AST
astArithmeticOp f [(ASTNumber a), (ASTNumber b)] = Just $ ASTNumber (f a b)
astArithmeticOp _ _ = Nothing

astComparisonOp :: (Int -> Int -> Bool) -> [AST] -> Maybe AST
astComparisonOp f [(ASTNumber a), (ASTNumber b)] = Just $ ASTBoolean (f a b)
astComparisonOp _ _ = Nothing

astIf :: [AST] -> Maybe AST
astIf [(ASTBoolean condition), a, b] = if condition then Just a else Just b
astIf _ = Nothing

evaluateAST' :: Symbols -> [AST] -> Maybe [AST]
evaluateAST' _ [] = Nothing
evaluateAST' f [x] = fmap singleton (fst $ evaluateAST1 f x)
evaluateAST' f (x : xs) = evaluated >>= (\x -> evaluateAST' symbols xs >>= (\xs -> Just (x : xs)))
    where (evaluated, symbols) = evaluateAST1 f x

evaluateAST :: [AST] -> Maybe [AST]
evaluateAST = evaluateAST' builtins

test :: [SExpr]
-- test = [ List [Symbol "define", Symbol "x", List [Symbol "+", Number 6, Number 5]]
        -- , List [Symbol "eq?", Number 1, List [Symbol "mod", List [Symbol "div", List [Symbol "*", Number 5, List [Symbol "+", Number 7, List [Symbol "-", Number 10, Number 2]]], Number 5], Number 7]]
        -- , List [Symbol "if", List [Symbol ">", Symbol "x", Number 8], Symbol "#t", Symbol "#f"] ]
test = [ List [Symbol "define", Symbol "a", Number 5], List [List [Lambda (List [Symbol "a", Symbol "b"]) (List [Symbol "+", Symbol "a", Symbol "b"])], List [Symbol "+", Symbol "a", Symbol "a"], Symbol "a"]]
-- test = [List [Symbol "define", Symbol "x", List [Lambda (List [Symbol "a", Symbol "b"]) (List [Symbol "+", Symbol "a", Symbol "b"])]], List [Symbol "x", Number 5, Number 10]]
-- test = [List [List [Lambda (List []) (Number 15)]]]

join :: String -> [String] -> String
join separator strings = foldr (\x y -> if null y then x else x ++ separator ++ y) "" strings

putMaybeStr :: Maybe String -> IO ()
putMaybeStr str = putStr $ fromMaybe "Nothing" str

putMaybeStrLn :: Maybe String -> IO ()
putMaybeStrLn str = putStrLn $ fromMaybe "Nothing" str

main :: IO ()
-- main = putStr $ join "\n" $ map (show . sexprToAST) test
main = putMaybeStrLn (fmap (\x -> join "\n" (map ((++) "Result: " . show) x)) (mapM sexprToAST (debug "SExpr: " test) >>= evaluateAST . debug "AST: "))
