module Main where

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

ex1 :: SExpr
ex1 = List [
    List [Symbol "x", Number 5],
    Symbol "x",
    List [Symbol "if", List [Symbol ">", Symbol "x", Number 4], Number 1, Number 0],
    List [Symbol "y", List [Symbol "+", Number 5, Symbol "x"]]]

getSymbol :: SExpr -> Maybe String
getSymbol (Symbol symbol) = Just symbol
getSymbol _ = Nothing

getInteger :: SExpr -> Maybe Int
getInteger (Number n) = Just n
getInteger _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (List exprs) = Just exprs
getList _ = Nothing

concatStrings :: Maybe String -> Maybe String -> Maybe String
concatStrings Nothing Nothing = Nothing
concatStrings Nothing other = other
concatStrings first Nothing = first
concatStrings (Just first) (Just other) = Just $ concat [first, other]

joinStrings :: String -> [Maybe String] -> Maybe String
joinStrings sep = foldr (\a b -> concatStrings a (concatStrings (b *> Just sep) b)) Nothing

printTree :: SExpr -> Maybe String
printTree (Number n) = Just $ concat ["a Number ", show n]
printTree (Symbol symbol) = Just $ concat ["a Symbol ", symbol]
printTree (List exprs) = concatStrings (Just "a List with ") (joinStrings " followed by " (map printTree exprs))

mainSExpr :: IO ()
mainSExpr = print $ printTree $ List [Symbol "define", Symbol "y", List [Symbol "*", Symbol "x", Number 5]]

fromSymbol :: SExpr -> String
fromSymbol (Symbol s) = s

data AST = ASTDefine String AST | ASTSymbol String | ASTNumber Int | ASTBoolean Bool | ASTCall String [AST] deriving Show

sexprToAST :: SExpr -> Maybe AST
sexprToAST (Number n) = Just $ ASTNumber n
sexprToAST (Symbol "#t") = Just $ ASTBoolean True
sexprToAST (Symbol "#f") = Just $ ASTBoolean False
sexprToAST (Symbol "define") = Nothing
sexprToAST (Symbol x) = Just $ ASTSymbol x
sexprToAST (List [(Symbol "define")]) = Nothing
sexprToAST (List [(Symbol "define"), (Symbol x), expr]) = sexprToAST expr >>= (\expr -> Just $ ASTDefine x expr)
sexprToAST (List ((Symbol "define") : xs)) = Nothing
sexprToAST (List ((Symbol x) : xs)) = mapM sexprToAST xs >>= (\xs -> Just $ ASTCall x xs)
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
traceSymbols f = (traceShowId $ map fst f) >> f

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

symbolId :: [AST] -> Maybe AST
symbolId [ast] = Just ast
symbolId _ = Nothing

evaluateAST1 :: Symbols -> AST -> (Maybe AST, Symbols)
evaluateAST1 symbols n@(ASTNumber _) = (Just n, symbols)
evaluateAST1 symbols b@(ASTBoolean _) = (Just b, symbols)
evaluateAST1 symbols (ASTSymbol s) = (find ((== s) . fst) symbols >>= (\(_, f) -> f [] >>= (fst . evaluateAST1 symbols)), symbols)
evaluateAST1 symbols (ASTCall f args) = ((find ((== f) . fst) symbols) >>= (\(s, f) -> evaluateAST' symbols args >>= f), symbols)
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
test = [ List [Symbol "define", Symbol "x", List [Symbol "+", Number 6, Number 5]]
        , List [Symbol "eq?", Number 1, List [Symbol "mod", List [Symbol "div", List [Symbol "*", Number 5, List [Symbol "+", Number 7, List [Symbol "-", Number 10, Number 2]]], Number 5], Number 7]]
        , List [Symbol "if", List [Symbol ">", Symbol "x", Number 8], Symbol "#t", Symbol "#f"] ]

join :: String -> [String] -> String
join separator strings = foldr (\x y -> if null y then x else x ++ separator ++ y) "" strings

putMaybeStr :: Maybe String -> IO ()
putMaybeStr str = putStr $ fromMaybe "Nothing" str

putMaybeStrLn :: Maybe String -> IO ()
putMaybeStrLn str = putStrLn $ fromMaybe "Nothing" str

main :: IO ()
-- main = putStr $ join "\n" $ map (show . sexprToAST) test
main = putMaybeStrLn (fmap (\x -> join "\n" (map show x)) (mapM sexprToAST test >>= evaluateAST))
