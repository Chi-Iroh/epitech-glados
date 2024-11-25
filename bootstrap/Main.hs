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

main :: IO ()
main = print $ sexprToAST $ List [Symbol "define", Symbol "x", List [Symbol "+", Number 6, Symbol "y"]]
