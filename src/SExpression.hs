module SExpression
    ( SExpr(SNumber, SSymbol, SList),
    parseStringToSExpr,
    getSymbol,
    getInteger,
    getList,
    concatStrings,
    joinStrings,
    printTree,
    mainSExpr,
    fromSymbol,
    AST(ASTDefine, ASTSymbol, ASTNumber, ASTBoolean, ASTCall),
    sexprToAST
    ) where

import Text.Read
import Data.Maybe
import Data.List (isPrefixOf)

data SExpr = SNumber Int | SSymbol String | SList [SExpr] deriving Show

data AlmostSExpr = ASExpr SExpr | SListBegin | SListEnd

-- take any string and return a SNumber if the string is an integer, return a SSymbol otherwise
convertToASExpr :: String -> [AlmostSExpr]
convertToASExpr [] = []
convertToASExpr str@(_:xs)
    | isPrefixOf "(" str = (SListBegin:(convertToASExpr xs))
    | isPrefixOf ")" (reverse str) = convertToASExpr (reverse (drop 1 (reverse str))) ++ [SListEnd]
    | isNothing (readMaybe str :: Maybe Int) = [ASExpr (SSymbol str)]
    | otherwise = [ASExpr (SNumber (fromJust $ readMaybe str))]

convertStringToASExprList :: [String] -> [AlmostSExpr] -> [AlmostSExpr]
convertStringToASExprList [] result = result
convertStringToASExprList (x:xs) result = convertStringToASExprList xs (result ++ (convertToASExpr x))

-- list [] 0
-- (le reste de la liste, la liste des paranthèses)
parseASEToSEParantheseHandlingB :: [AlmostSExpr] -> [AlmostSExpr] -> Int -> Maybe ([AlmostSExpr], [AlmostSExpr])
parseASEToSEParantheseHandlingB [] _ _ = Nothing
parseASEToSEParantheseHandlingB (SListEnd:rList) pList 0 = Just (rList, reverse pList)
parseASEToSEParantheseHandlingB (SListEnd:rList) pList i = parseASEToSEParantheseHandlingB rList (SListEnd:pList) (i - 1)
parseASEToSEParantheseHandlingB (SListBegin:rList) pList i = parseASEToSEParantheseHandlingB rList (SListBegin:pList) (i + 1)
parseASEToSEParantheseHandlingB (r:rList) pList i = parseASEToSEParantheseHandlingB rList (r:pList) i

fromMaybeSExprListToSList :: Maybe [SExpr] -> Maybe SExpr
fromMaybeSExprListToSList (Just list) = Just (SList list)
fromMaybeSExprListToSList _ = Nothing

addMaybeSExprToSExprList :: Maybe SExpr -> Maybe [SExpr] -> Maybe [SExpr]
addMaybeSExprToSExprList (Just e) (Just es) = Just (e:es)
addMaybeSExprToSExprList _ _ = Nothing

parseASEToSEParantheseHandlingA :: Maybe ([AlmostSExpr], [AlmostSExpr]) -> Maybe [SExpr] -> Maybe [SExpr]
parseASEToSEParantheseHandlingA (Just (rList, pList)) list = parseASExprListToSExprList rList (addMaybeSExprToSExprList (fromMaybeSExprListToSList (parseASExprListToSExprList pList (Just []))) list)
parseASEToSEParantheseHandlingA _ _ = Nothing

parseASExprListToSExprList :: [AlmostSExpr] -> Maybe [SExpr] -> Maybe [SExpr]
parseASExprListToSExprList _ Nothing = Nothing
parseASExprListToSExprList [] (Just list) = Just (reverse list)
parseASExprListToSExprList ((ASExpr expr):xs) (Just list) = parseASExprListToSExprList xs (Just (expr:list))
parseASExprListToSExprList (SListEnd:_) _ = Nothing
parseASExprListToSExprList (SListBegin:xs) list = parseASEToSEParantheseHandlingA (parseASEToSEParantheseHandlingB xs [] 0) list

parseStringToSExpr :: String -> Maybe SExpr
parseStringToSExpr str = fromMaybeSExprListToSList (parseASExprListToSExprList (convertStringToASExprList (words str) []) (Just []))

getSymbol :: SExpr -> Maybe String
getSymbol (SSymbol symbol) = Just symbol
getSymbol _ = Nothing

getInteger :: SExpr -> Maybe Int
getInteger (SNumber n) = Just n
getInteger _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (SList exprs) = Just exprs
getList _ = Nothing

concatStrings :: Maybe String -> Maybe String -> Maybe String
concatStrings Nothing Nothing = Nothing
concatStrings Nothing other = other
concatStrings first Nothing = first
concatStrings (Just first) (Just other) = Just $ concat [first, other]

joinStrings :: String -> [Maybe String] -> Maybe String
joinStrings sep = foldr (\a b -> concatStrings a (concatStrings (b *> Just sep) b)) Nothing

printTree :: SExpr -> Maybe String
printTree (SNumber n) = Just $ concat ["a Number ", show n]
printTree (SSymbol symbol) = Just $ concat ["a Symbol ", symbol]
printTree (SList exprs) = concatStrings (Just "a List with ") (joinStrings " followed by " (map printTree exprs))

mainSExpr :: IO ()
mainSExpr = print $ printTree $ SList [SSymbol "define", SSymbol "y", SList [SSymbol "*", SSymbol "x", SNumber 5]]

fromSymbol :: SExpr -> String
fromSymbol (SSymbol s) = s
fromSymbol _ = ""

data AST = ASTDefine String AST | ASTSymbol String | ASTNumber Int | ASTBoolean Bool | ASTCall String [AST] deriving Show

sexprToAST :: SExpr -> Maybe AST
sexprToAST (SNumber n) = Just $ ASTNumber n
sexprToAST (SSymbol "#t") = Just $ ASTBoolean True
sexprToAST (SSymbol "#f") = Just $ ASTBoolean False
sexprToAST (SSymbol "define") = Nothing
sexprToAST (SSymbol x) = Just $ ASTSymbol x
sexprToAST (SList []) = Nothing
sexprToAST (SList [(SSymbol "define")]) = Nothing
sexprToAST (SList [(SSymbol "define"), (SSymbol x), expr]) = sexprToAST expr >>= (\expr -> Just $ ASTDefine x expr)
sexprToAST (SList ((SSymbol x) : xs))
    | x == "define" = Nothing
    | otherwise = mapM sexprToAST xs >>= (\xs -> Just $ ASTCall x xs)
