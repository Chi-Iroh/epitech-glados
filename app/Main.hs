module Main (main) where

import Converter
import Evaluate (evaluateAST)
import Parser
import System.Exit
import System.Environment
import Utils

getFileName :: [String] -> Maybe String
getFileName [a] = Just a
getFileName _ = Nothing

toSafe :: Maybe a -> Safe a
toSafe Nothing = Error "Happy debugging ^^"
toSafe (Just a) = Value a

main :: IO ()
main = do
    args <- getArgs
    filename <- case getFileName args of
                Nothing -> exitWith(ExitFailure 84)
                Just filename -> pure filename
    file <- readFile filename
    print ((convert $ parse file) >>= (toSafe . evaluateAST))
