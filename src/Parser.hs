module Parser (
    AlmostSExpr(ASExpr, SListBegin, SListEnd),
    convertToASExpr,
    stringToASExpr,
    parseParanthese,
    fromSafe,
    concatSafe,
    verifyParanthese,
    aSExprToSExpr,
    parse
    ) where

import Text.Read
import Data.Maybe
import Data.List (isPrefixOf)

import SExpression
import Utils

data AlmostSExpr = ASExpr SExpr | SListBegin | SListEnd deriving (Eq, Show)

-- take any string and return a SNumber if the string is an integer, return a SSymbol otherwise
convertToASExpr :: String -> [AlmostSExpr]
convertToASExpr [] = []
convertToASExpr str@(_:xs)
    | isPrefixOf "(" str = (SListBegin:(convertToASExpr xs))
    | isPrefixOf ")" (reverse str) = convertToASExpr (reverse (drop 1 (reverse str))) ++ [SListEnd]
    | isNothing (readMaybe str :: Maybe Int) = [ASExpr (SSymbol str)]
    | otherwise = [ASExpr (SNumber (fromJust $ readMaybe str))]

stringToASExpr :: [String] -> [AlmostSExpr] -> [AlmostSExpr]
stringToASExpr [] result = result
stringToASExpr (x:xs) result = stringToASExpr xs (result ++ (convertToASExpr x))

-- list [] 0
-- (le reste de la liste, la liste des paranthèses)
parseParanthese :: [AlmostSExpr] -> [AlmostSExpr] -> Int -> Safe ([AlmostSExpr], [AlmostSExpr])
parseParanthese [] _ _ = Error "GLaDOS: SyntaxError: unexpected EOF while parsing, ')' expected\n"
parseParanthese (SListEnd:rList) pList 0 = Value (rList, reverse pList)
parseParanthese (SListEnd:rList) pList i = parseParanthese rList (SListEnd:pList) (i - 1)
parseParanthese (SListBegin:rList) pList i = parseParanthese rList (SListBegin:pList) (i + 1)
parseParanthese (r:rList) pList i = parseParanthese rList (r:pList) i

fromSafe :: Safe [SExpr] -> Safe SExpr
fromSafe (Value list) = Value (SList list)
fromSafe (Error err) = Error err

concatSafe :: Safe SExpr -> Safe [SExpr] -> Safe [SExpr]
concatSafe (Value e) (Value es) = Value (e:es)
concatSafe (Error err) _ = Error err
concatSafe _ (Error err) = Error err

verifyParanthese :: Safe ([AlmostSExpr], [AlmostSExpr]) -> Safe [SExpr] -> Safe [SExpr]
verifyParanthese (Value (rList, pList)) list = aSExprToSExpr rList (concatSafe (fromSafe (aSExprToSExpr pList (Value []))) list)
verifyParanthese (Error err) _ = Error err

aSExprToSExpr :: [AlmostSExpr] -> Safe [SExpr] -> Safe [SExpr]
aSExprToSExpr _ (Error err) = Error err
aSExprToSExpr [] (Value list) = Value (reverse list)
aSExprToSExpr ((ASExpr expr):xs) (Value list) = aSExprToSExpr xs (Value (expr:list))
aSExprToSExpr (SListEnd:_) _ = Error "GLaDOS: SyntaxError: unexpected ')' while parsing\n"
aSExprToSExpr (SListBegin:xs) list = verifyParanthese (parseParanthese xs [] 0) list

parse :: String -> Safe [SExpr]
parse str = aSExprToSExpr (stringToASExpr (words str) []) (Value [])
