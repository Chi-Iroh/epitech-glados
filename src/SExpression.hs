module SExpression
    ( SExpr(SNumber, SSymbol, SList, STuple, SArray),
    getSymbol,
    getInteger,
    getList,
    printTree,
    mainSExpr,
    fromSymbol
    ) where

import Utils
--import Type

data SExpr = SNumber Int | SSymbol String | SList [SExpr] | STuple [SExpr] | SArray [SExpr] deriving (Eq, Show)

getSymbol :: SExpr -> Safe String
getSymbol (SSymbol symbol) = Value symbol
getSymbol _ = Error "SExpr is not a SSymbol."

getInteger :: SExpr -> Safe Int
getInteger (SNumber n) = Value n
getInteger _ = Error "SExpr is not a SNumber."

getList :: SExpr -> Safe [SExpr]
getList (SList exprs) = Value exprs
getList _ = Error "SExpr is not a SList."

printTree :: SExpr -> Safe String
printTree (SNumber n) = Value $ concat ["a Number ", show n]
printTree (SSymbol symbol) = Value $ concat ["a Symbol ", symbol]
printTree (SList exprs) = concatSStrings (Value "a List with ") (joinSStrings " followed by " (map printTree exprs))

mainSExpr :: IO ()
mainSExpr = print $ printTree $ SList [SSymbol "define", SSymbol "y", SList [SSymbol "*", SSymbol "x", SNumber 5]]

fromSymbol :: SExpr -> String
fromSymbol (SSymbol s) = s
fromSymbol _ = ""
