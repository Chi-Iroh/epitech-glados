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

parse :: String -> Maybe SExpr
parse str = fromMaybeSExprListToSList (parseASExprListToSExprList (convertStringToASExprList (words str) []) (Just []))
