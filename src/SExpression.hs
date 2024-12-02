module SExpression
    ( SExpr(SNumber, SSymbol, SList),
    getSymbol,
    getInteger,
    getList,
    printTree,
    mainSExpr,
    fromSymbol,
    checkError
    ) where

import System.Exit
import Utils

data SExpr = SNumber Int | SSymbol String | SList [SExpr] deriving (Eq, Show)

getSymbol :: SExpr -> Maybe String
getSymbol (SSymbol symbol) = Just symbol
getSymbol _ = Nothing

getInteger :: SExpr -> Maybe Int
getInteger (SNumber n) = Just n
getInteger _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (SList exprs) = Just exprs
getList _ = Nothing

printTree :: SExpr -> Maybe String
printTree (SNumber n) = Just $ concat ["a Number ", show n]
printTree (SSymbol symbol) = Just $ concat ["a Symbol ", symbol]
printTree (SList exprs) = concatMBStrings (Just "a List with ") (joinMBStrings " followed by " (map printTree exprs))

mainSExpr :: IO ()
mainSExpr = print $ printTree $ SList [SSymbol "define", SSymbol "y", SList [SSymbol "*", SSymbol "x", SNumber 5]]

fromSymbol :: SExpr -> String
fromSymbol (SSymbol s) = s
fromSymbol _ = ""

--either
checkError :: Safe SExpr -> IO ()
checkError (Value e) = print e
checkError (Error err) = print err >> exitWith(ExitFailure 84)
