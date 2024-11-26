module Parser (parse) where

--collectUntilClosing :: String -> (Maybe String, String)
--collectUntilClosing [] = (Nothing, [])
--collectUntilClosing (a:as)
--        | a == '(' =
--            let (inner, rest) = collectUntilClosing as
--            in case inner of
--                Just content    ->
--                    let (nested, finalRest) = collectUntilClosing rest
--                    in case nested of
--                        Just nestedContent  -> (Just ('(':content ++ nestedContent), finalRest)
--                        Nothing             -> (Just ('(':content), rest)
--                Nothing         -> (Nothing, rest)
--        | a == ')' = (Just [a], as)
--        | otherwise =
--            let (inner, rest) = collectUntilClosing as
--            in case inner of
--                Just content    -> (Just (a: content), rest)
--                Nothing         -> (Nothing, rest)


--splitByParanthese :: String -> (Maybe String, String)
--splitByParanthese [] = (Nothing, [])
--splitByParanthese (a:as)
--        | a == '(' =
--            let (inner, rest) = collectUntilClosing as
--            in case inner of
--                Just content    -> (Just ('(' : content), rest)
--                Nothing         -> (Nothing, rest)
--        | otherwise = error "unwanted text before first open paranthese"

import Text.Read
import Data.Maybe
import Data.List (isPrefixOf)

import SExpression
import Utils

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
parseASEToSEParantheseHandlingB :: [AlmostSExpr] -> [AlmostSExpr] -> Int -> Safe ([AlmostSExpr], [AlmostSExpr])
parseASEToSEParantheseHandlingB [] _ _ = Error "GLaDOS: SyntaxError: unexpected EOF while parsing, ')' expected"
parseASEToSEParantheseHandlingB (SListEnd:rList) pList 0 = Value (rList, reverse pList)
parseASEToSEParantheseHandlingB (SListEnd:rList) pList i = parseASEToSEParantheseHandlingB rList (SListEnd:pList) (i - 1)
parseASEToSEParantheseHandlingB (SListBegin:rList) pList i = parseASEToSEParantheseHandlingB rList (SListBegin:pList) (i + 1)
parseASEToSEParantheseHandlingB (r:rList) pList i = parseASEToSEParantheseHandlingB rList (r:pList) i

fromMaybeSExprListToSList :: Safe [SExpr] -> Safe SExpr
fromMaybeSExprListToSList (Value list) = Value (SList list)
fromMaybeSExprListToSList (Error err) = Error err

addMaybeSExprToSExprList :: Safe SExpr -> Safe [SExpr] -> Safe [SExpr]
addMaybeSExprToSExprList (Value e) (Value es) = Value (e:es)
addMaybeSExprToSExprList (Error err) _ = Error err
addMaybeSExprToSExprList _ (Error err) = Error err

parseASEToSEParantheseHandlingA :: Safe ([AlmostSExpr], [AlmostSExpr]) -> Safe [SExpr] -> Safe [SExpr]
parseASEToSEParantheseHandlingA (Value (rList, pList)) list = parseASExprListToSExprList rList (addMaybeSExprToSExprList (fromMaybeSExprListToSList (parseASExprListToSExprList pList (Value []))) list)
parseASEToSEParantheseHandlingA (Error err) _ = Error err
parseASEToSEParantheseHandlingA _ (Error err) = Error err

parseASExprListToSExprList :: [AlmostSExpr] -> Safe [SExpr] -> Safe [SExpr]
parseASExprListToSExprList _ (Error err) = Error err
parseASExprListToSExprList [] (Value list) = Value (reverse list)
parseASExprListToSExprList ((ASExpr expr):xs) (Value list) = parseASExprListToSExprList xs (Value (expr:list))
parseASExprListToSExprList (SListEnd:_) _ = Error "GLaDOS: SyntaxError: unexpected ')' while parsing"
parseASExprListToSExprList (SListBegin:xs) list = parseASEToSEParantheseHandlingA (parseASEToSEParantheseHandlingB xs [] 0) list

parse :: String -> Safe SExpr
parse str = fromMaybeSExprListToSList (parseASExprListToSExprList (convertStringToASExprList (words str) []) (Value []))
