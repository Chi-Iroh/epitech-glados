module Evaluate (evaluateAST, Symbol(..), Symbols, symbolName) where

import AST (AST(..), Call(..))
import Data.List (singleton)

builtins :: Symbols
builtins = [BackendSymbol ("*", astArithmeticOp (*))
            , BackendSymbol ("+", astArithmeticOp (+))
            , BackendSymbol ("-", astArithmeticOp (-))
            , BackendSymbol ("div", astArithmeticOp div)
            , BackendSymbol ("mod", astArithmeticOp mod)
            , BackendSymbol (">", astComparisonOp (>))
            , BackendSymbol (">=", astComparisonOp (>=))
            , BackendSymbol ("<", astComparisonOp (<))
            , BackendSymbol ("<=", astComparisonOp (<=))
            , BackendSymbol ("eq?", astComparisonOp (==))
            , BackendSymbol ("==", astComparisonOp (==))
            , BackendSymbol ("if", astIf)]

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f (x : xs) = if (f x) then Just x else find f xs

data Symbol = BackendSymbol (String, (Symbols -> [AST] -> Maybe AST))
type Symbols = [Symbol]

symbolName :: Symbol -> String
symbolName (BackendSymbol (s, _)) = s

updateOrAdd :: (a -> Bool) -> a -> [a] -> [a]
updateOrAdd _ a [] = [a]
updateOrAdd f a (x : xs)
    | f x = a : xs
    | otherwise = x : updateOrAdd f a xs

registerSymbol :: Symbols -> String -> (Symbols -> [AST] -> Maybe AST) -> Symbols
registerSymbol symbols name f = updateOrAdd ((== name) . symbolName) (BackendSymbol (name, f)) symbols

symbolId :: AST -> Symbols -> [AST] -> Maybe AST
symbolId value _ [] = Just value
symbolId _ _ _ = Nothing

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
evaluateAST1 symbols (ASTSymbol s) = (find ((== s) . symbolName) symbols >>= (\(BackendSymbol (_, f)) -> f symbols [] >>= (fst . evaluateAST1 symbols)), symbols)
evaluateAST1 symbols (ASTCall (FunctionCall f) args) = (find ((== f) . symbolName) symbols >>= (\(BackendSymbol (_, f')) -> f' symbols args), symbols)
evaluateAST1 symbols (ASTCall (LambdaCall params body) args) = (args' >>= (makeLambdaSymbols symbols params) >>= (\symbols' -> fst $ evaluateAST1 symbols' body), symbols) -- discarding lambda symbols and returning old ones
    where args' = if null args then Just [] else (mapM (fst . evaluateAST1 symbols) args)
evaluateAST1 symbols define@(ASTDefine s (ASTLambda params body)) = (Just define, registerSymbol symbols s function)
    where function symbols' args = mapM (fst . evaluateAST1 symbols') args >>= (\args' -> fst $ evaluateAST1 symbols' (ASTCall (LambdaCall params body) args'))
evaluateAST1 symbols define@(ASTDefine s ast) = (Just define, registerSymbol symbols s function)
    where function symbols' args
            | null args = Just ast
            | length args >= 2 = Nothing
            | otherwise = fst $ evaluateAST1 symbols' $ head args

astArithmeticOp' :: (Int -> Int -> Int) -> [AST] -> Maybe AST
astArithmeticOp' f [(ASTNumber a), (ASTNumber b)] = Just $ ASTNumber (f a b)
astArithmeticOp' _ _ = Nothing

astArithmeticOp :: (Int -> Int -> Int) -> Symbols -> [AST] -> Maybe AST
astArithmeticOp f symbols args = mapM (fst . evaluateAST1 symbols) args >>= astArithmeticOp' f

toNumber :: AST -> Maybe Int
toNumber (ASTBoolean b) = Just (fromEnum b)
toNumber (ASTNumber n) = Just n
toNumber _ = Nothing

astComparisonOp' :: (Int -> Int -> Bool) -> [AST] -> Maybe AST
astComparisonOp' f args@[_, _] = mapM toNumber args >>= compare'
    where compare' [a', b'] = Just $ ASTBoolean (f a' b')
          compare' _ = Nothing
astComparisonOp' _ _ = Nothing

astComparisonOp :: (Int -> Int -> Bool) -> Symbols -> [AST] -> Maybe AST
astComparisonOp f symbols args = mapM (fst . evaluateAST1 symbols) args >>= astComparisonOp' f

astIf' :: Symbols -> [AST] -> Maybe AST
astIf' symbols [(ASTBoolean condition), a, b] = if condition then fst (evaluateAST1 symbols a) else fst (evaluateAST1 symbols b)
astIf' _ _ = Nothing

astIf :: Symbols -> [AST] -> Maybe AST
astIf symbols [a, b, c] = (fst $ evaluateAST1 symbols a) >>= (\a' -> astIf' symbols [a', b, c])
astIf _ _ = Nothing

evaluateAST' :: Symbols -> [AST] -> Maybe [AST]
evaluateAST' _ [] = Nothing
evaluateAST' f [x] = fmap singleton (fst $ evaluateAST1 f x)
evaluateAST' f (x : xs) = evaluated >>= (\x' -> evaluateAST' symbols xs >>= (\xs' -> Just (x' : xs')))
    where (evaluated, symbols) = evaluateAST1 f x

evaluateAST :: [AST] -> Maybe [AST]
evaluateAST = evaluateAST' builtins

-- test :: [SExpr]
-- test = [ List [Symbol "define", Symbol "x", List [Symbol "+", Number 6, Number 5]]
        -- , List [Symbol "eq?", Number 1, List [Symbol "mod", List [Symbol "div", List [Symbol "*", Number 5, List [Symbol "+", Number 7, List [Symbol "-", Number 10, Number 2]]], Number 5], Number 7]]
        -- , List [Symbol "if", List [Symbol ">", Symbol "x", Number 8], Symbol "#t", Symbol "#f"] ]
-- test = [ List [Symbol "define", Symbol "a", Number 5], List [List [Lambda (List [Symbol "a", Symbol "b"]) (List [Symbol "+", Symbol "a", Symbol "b"])], List [Symbol "+", Symbol "a", Symbol "a"], Symbol "a"]]
-- test = [List [Symbol "define", Symbol "x", List [Lambda (List [Symbol "a", Symbol "b"]) (List [Symbol "+", Symbol "a", Symbol "b"])]], List [Symbol "x", Number 5, Number 10]]
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
