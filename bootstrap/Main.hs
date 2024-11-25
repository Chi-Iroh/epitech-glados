module Main where

data SExpr = Number Int | Symbol String | List [SExpr] deriving Show

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
sexprToAST (List []) = Nothing
sexprToAST (List [(Symbol "define")]) = Nothing
sexprToAST (List [(Symbol "define"), (Symbol x), expr]) = sexprToAST expr >>= (\expr -> Just $ ASTDefine x expr)
sexprToAST (List ((Symbol x) : xs))
    | x == "define" = Nothing
    | otherwise = mapM sexprToAST xs >>= (\xs -> Just $ ASTCall x xs)

builtins :: [String]
builtins = ["<", ">", "eq?", "*", "+", "-", "div", "mod", "if"]

isBuiltin :: String -> Bool
isBuiltin x = elem x builtins

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f (x : xs) = if (f x) then Just x else find f xs

evaluateAST' :: [(String, ([AST] -> Maybe AST))] -> AST -> Maybe AST
evaluateAST' _ n@(ASTNumber _) = Just n
evaluateAST' _ b@(ASTBoolean _) = Just b
evaluateAST' _ define@(ASTDefine _ _) = Just define
evaluateAST' symbols (ASTSymbol s) = find ((== s) . fst) symbols >>= (\(_, f) -> f [])
evaluateAST' symbols (ASTCall f args) = find ((== f) . fst) symbols >>= (\(_, f) -> mapM (evaluateAST' symbols) args >>= f)

astArithmeticOp :: (Int -> Int -> Int) -> [AST] -> Maybe AST
astArithmeticOp f [(ASTNumber a), (ASTNumber b)] = Just $ ASTNumber (f a b)
astArithmeticOp _ _ = Nothing

astComparisonOp :: (Int -> Int -> Bool) -> [AST] -> Maybe AST
astComparisonOp f [(ASTNumber a), (ASTNumber b)] = Just $ ASTBoolean (f a b)
astComparisonOp _ _ = Nothing

astIf :: [AST] -> Maybe AST
astIf [(ASTBoolean condition), a, b] = if condition then Just a else Just b
astIf _ = Nothing

evaluateAST :: AST -> Maybe AST
evaluateAST = evaluateAST' [("*", astArithmeticOp (*)), ("+", astArithmeticOp (+)), ("-", astArithmeticOp (-)), ("div", astArithmeticOp div), ("mod", astArithmeticOp mod), (">", astComparisonOp (>)), ("<", astComparisonOp (<)), ("eq?", astComparisonOp (==)), ("if", astIf)]

main :: IO ()
-- main = print $ sexprToAST $ List [Symbol "define", Symbol "x", List [Symbol "+", Number 6, Symbol "y"]]
-- main = print $ ((sexprToAST $ List [Symbol "eq?", Number 1, List [Symbol "mod", List [Symbol "div", List [Symbol "*", Number 5, List [Symbol "+", Number 7, List [Symbol "-", Number 10, Number 2]]], Number 5], Number 7]]) >>= evaluateAST)
main = print $ ((sexprToAST $ List [Symbol "if", List [Symbol ">", Number 10, Number 8], Symbol "#t", Symbol "#f"]) >>= evaluateAST)
