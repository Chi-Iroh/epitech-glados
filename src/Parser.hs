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

-- Parses a tuple structure.
parseTuple :: [AlmostSExpr] -> [AlmostSExpr] -> Int -> Safe ([AlmostSExpr], [AlmostSExpr])
parseTuple [] _ _ = Error "GLaDOS: SyntaxError: unexpected EOF while parsing, '}' expected\n"
parseTuple (STupleEnd:rList) pList 0 = Value (rList, reverse pList)
parseTuple (STupleEnd:rList) pList i = parseTuple rList (STupleEnd:pList) (i - 1)
parseTuple (STupleBegin:rList) pList i = parseTuple rList (STupleBegin:pList) (i + 1)
parseTuple (r:rList) pList i = parseTuple rList (r:pList) i

-- Parses an array structure.
parseArray :: [AlmostSExpr] -> [AlmostSExpr] -> Int -> Safe ([AlmostSExpr], [AlmostSExpr])
parseArray [] _ _ = Error "GLaDOS: SyntaxError: unexpected EOF while parsing, ']' expected\n"
parseArray (SArrayEnd:rList) pList 0 = Value (rList, reverse pList)
parseArray (SArrayEnd:rList) pList i = parseArray rList (SArrayEnd:pList) (i - 1)
parseArray (SArrayBegin:rList) pList i = parseArray rList (SArrayBegin:pList) (i + 1)
parseArray (r:rList) pList i = parseArray rList (r:pList) i

fromSafe :: Safe [SExpr] -> Safe SExpr
fromSafe (Value list) = Value (SList list)
fromSafe (Error err) = Error err

fromSafeTuple :: Safe [SExpr] -> Safe SExpr
fromSafeTuple (Value list) = Value (STuple list)
fromSafeTuple (Error err) = Error err

fromSafeArray :: Safe [SExpr] -> Safe SExpr
fromSafeArray (Value list) = Value (SArray list)
fromSafeArray (Error err) = Error err

concatSafe :: Safe SExpr -> Safe [SExpr] -> Safe [SExpr]
concatSafe (Value e) (Value es) = Value (e:es)
concatSafe (Error err1) (Error err2) = Error ("2 Errors encountered at the same time: " ++ err1 ++ " ; " ++ err2)
concatSafe (Error err) _ = Error err
concatSafe _ (Error err) = Error err

verifyParanthese :: Safe ([AlmostSExpr], [AlmostSExpr]) -> Safe [SExpr] -> Safe [SExpr]
verifyParanthese (Value (rList, pList)) list = aSExprToSExpr rList (concatSafe (fromSafe (aSExprToSExpr pList (Value []))) list)
verifyParanthese (Error err) _ = Error err

-- return a list of AlmostSexpr trunc to the first SListEnd (and trunc it)
toSListEnd :: [AlmostSExpr] -> [AlmostSExpr]
toSListEnd [] = []
toSListEnd (x:xs)
    | x == SListEnd = xs
    | otherwise = toSListEnd xs

-- return a list of AlmostSexpr trunc to the first STupleEnd (and trunc it)
toSTupleEnd :: [AlmostSExpr] -> [AlmostSExpr]
toSTupleEnd [] = []
toSTupleEnd (x:xs)
    | x == STupleEnd = xs
    | otherwise = toSTupleEnd xs

-- return a list of AlmostSexpr trunc to the first STupleEnd (and trunc it)
toSArrayEnd :: [AlmostSExpr] -> [AlmostSExpr]
toSArrayEnd [] = []
toSArrayEnd (x:xs)
    | x == SArrayEnd = xs
    | otherwise = toSArrayEnd xs

-- bollean for checkValidTuple
isValidTuple :: Int -> [AlmostSExpr] -> Bool
isValidTuple _ [] = True
isValidTuple nbElement (x:xs)
    | nbElement > 2 = False
    | otherwise =
        case x of
            SListBegin -> isValidTuple nbElement (toSListEnd xs)
            STupleBegin -> isValidTuple nbElement (toSTupleEnd xs)
            SArrayBegin -> isValidTuple nbElement (toSArrayEnd xs)
            ASExpr (SSymbol ",") -> isValidTuple (nbElement + 1) xs
            _ -> isValidTuple nbElement xs

-- return a Tuple if is a Valid, else return an Err
checkValidTuple :: [AlmostSExpr] -> Safe [AlmostSExpr]
checkValidTuple list
    | isValidTuple 1 list = Value list
    | otherwise = Error "Invalid tuple detected"

verifyTuple :: Safe ([AlmostSExpr], [AlmostSExpr]) -> Safe [SExpr] -> Safe [SExpr]
verifyTuple (Value (rList, pList)) list =
    trace ("rlist: " ++ show rList ++ "\npList " ++ show pList ++ "list" ++ show list) $
    let tuple = checkValidTuple pList
    in case tuple of
        Value validTuple -> 
            aSExprToSExpr rList (concatSafe (fromSafeTuple (aSExprToSExpr validTuple (Value []))) list)
        Error err -> 
            Error err
verifyTuple (Error err) _ = Error err

verifyArray :: Safe ([AlmostSExpr], [AlmostSExpr]) -> Safe [SExpr] -> Safe [SExpr]
verifyArray (Value (rList, pList)) list =
    aSExprToSExpr rList (concatSafe (fromSafeArray (aSExprToSExpr pList (Value []))) list)
verifyArray (Error err) _ = Error err

aSExprToSExpr :: [AlmostSExpr] -> Safe [SExpr] -> Safe [SExpr]
aSExprToSExpr _ (Error err) = Error err
aSExprToSExpr [] (Value list) = Value (reverse list)
aSExprToSExpr ((ASExpr expr):xs) (Value list) = aSExprToSExpr xs (Value (expr:list))
aSExprToSExpr (SListEnd:_) _ = Error "GLaDOS: SyntaxError: unexpected ')' while parsing\n"
aSExprToSExpr (SListBegin:xs) list = verifyParanthese (parseParanthese xs [] 0) list
aSExprToSExpr (STupleBegin:xs) list = verifyTuple (parseTuple xs [] 0) list
aSExprToSExpr (STupleEnd:_) _ = Error "GLaDOS: SyntaxError: unexpected '}' while parsing\n"
aSExprToSExpr (SArrayBegin:xs) list = verifyArray (parseArray xs [] 0) list
aSExprToSExpr (SArrayEnd:_) _ = Error "GLaDOS: SyntaxError: unexpected ']' while parsing\n"

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
                Value (_, list) ->
                    -- trace ("list: " ++ show list) $
                    aSExprToSExpr list (Value [])
                Error err -> Error err