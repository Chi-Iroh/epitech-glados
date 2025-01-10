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
import Debug.Trace (trace)
import Data.List (isPrefixOf)

import SExpression
import Utils

data AlmostSExpr = ASExpr SExpr | SListBegin | SListEnd | STupleBegin | STupleEnd | SArrayBegin | SArrayEnd deriving (Eq, Show)

-- take any string and return a SNumber if the string is an integer, return a SSymbol otherwise
convertToASExpr :: String -> [AlmostSExpr]
convertToASExpr [] = []
convertToASExpr str@(_:xs)
    | isPrefixOf "(" str = (SListBegin:(convertToASExpr xs))
    | isPrefixOf ")" (reverse str) = convertToASExpr (reverse (drop 1 (reverse str))) ++ [SListEnd]
    | isPrefixOf "{" str = (STupleBegin:(convertToASExpr xs))
    | isPrefixOf "}" (reverse str) = convertToASExpr (reverse (drop 1 (reverse str))) ++ [STupleEnd]
    | isPrefixOf "[" str = (SArrayBegin:(convertToASExpr xs))
    | isPrefixOf "]" (reverse str) = convertToASExpr (reverse (drop 1 (reverse str))) ++ [SArrayEnd]
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
concatSafe (Error err1) (Error err2) = Error ("2 Errors encountered at the same time: " ++ err1 ++ " ; " ++ err2)
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
-- aSExprToSExpr (STupleBegin:xs) tuple = verifyTuple (parseTuple xs [] 0 ) tuple
-- aSExprToSExpr (STupleEnd:_) _ = Error "GLaDOS: SyntaxError: unexpected '}' while parsing\n"
-- a

-- recursive function for check if array, tuple, etc.. are not interlocked    
verifyASExpr :: Maybe Char -> Int -> [AlmostSExpr] -> Safe (Int, [AlmostSExpr])
verifyASExpr char index list
    | index > length list - 1 = Value (index, list)
    | otherwise =
        -- trace ("char: " ++ show char) $
        -- trace ("index: " ++ show index) $
        -- trace ("list: " ++ show list) $
        -- trace ("\n") $
        if charNothing
            then
                let currentElement = list !! index
                in case currentElement of
                    SListBegin ->
                        let result = verifyASExpr (Just '(') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr Nothing (returnIndex + 1) list
                            Error err -> Error err
                    STupleBegin ->
                        let result = verifyASExpr (Just '{') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr Nothing (returnIndex + 1) list
                            Error err -> Error err
                    SArrayBegin ->
                        let result = verifyASExpr (Just '[') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr Nothing (returnIndex + 1) list
                            Error err -> Error err
                    SListEnd -> Error "SyntaxError: Unexpecting closing paranthese found"
                    STupleEnd -> Error "SyntaxError: Unexpecting closing curly bracket found"
                    SArrayEnd -> Error "SyntaxError: Unexpecting closing bracket found"
                    _ ->  verifyASExpr Nothing (index + 1) list
        else if charParanthese
            then
                let currentElement = list !! index
                in case currentElement of
                    SListBegin ->
                        let result = verifyASExpr (Just '(') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just '(') (returnIndex + 1) list
                            Error err -> Error err
                    STupleBegin ->
                        let result = verifyASExpr (Just '{') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just '(') (returnIndex + 1) list
                            Error err -> Error err
                    SArrayBegin ->
                        let result = verifyASExpr (Just '[') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just '(') (returnIndex + 1) list
                            Error err -> Error err
                    SListEnd -> Value (index, list)
                    STupleEnd -> Error "SyntaxError: Interlocked Tuple in ()"
                    SArrayEnd -> Error "SyntaxError: Interlocked Array in ()"
                    _ -> verifyASExpr (Just '(')  (index + 1) list
        else if charCurlyBrack
            then
                let currentElement = list !! index
                in case currentElement of
                    SListBegin ->
                        let result = verifyASExpr (Just '(') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just '{') (returnIndex + 1) list
                            Error err -> Error err
                    STupleBegin ->
                        let result = verifyASExpr (Just '{') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just '{') (returnIndex + 1) list
                            Error err -> Error err
                    SArrayBegin ->
                        let result = verifyASExpr (Just '[') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just '{') (returnIndex + 1) list
                            Error err -> Error err
                    SListEnd -> Error "SyntaxError: Interlocked () in Tuple"
                    STupleEnd -> Value (index, list)
                    SArrayEnd -> Error "SyntaxError: Interlocked Array in Tuple"
                    _ -> verifyASExpr (Just '{')  (index + 1) list
        else if charBrack
            then
            let currentElement = list !! index
                in case currentElement of
                    SListBegin ->
                        let result = verifyASExpr (Just '(') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just '[') (returnIndex + 1) list
                            Error err -> Error err
                    STupleBegin ->
                        let result = verifyASExpr (Just '{') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just '[') (returnIndex + 1) list
                            Error err -> Error err
                    SArrayBegin ->
                        let result = verifyASExpr (Just '[') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just '[') (returnIndex + 1) list
                            Error err -> Error err
                    SListEnd -> Error "SyntaxError: Interlocked () in Array"
                    STupleEnd -> Error "SyntaxError: Interlocked Tuple in Array"
                    SArrayEnd -> Value (index, list)
                    _ -> verifyASExpr (Just '[')  (index + 1) list
        else
            error "You unlock an achivement because this is impossible. Unexpected character passes as argument"
    where
        charNothing = isNothing char
        charParanthese = char == Just '('
        charCurlyBrack = char == Just '{'
        charBrack = char == Just '['

parse :: String -> Safe [SExpr]
parse str = let result = verifyASExpr Nothing 0 (stringToASExpr (words str) [])
            in case result of
                Value (_, list) -> trace ("list: " ++ show list) $
                    aSExprToSExpr list (Value [])
                Error err -> Error err