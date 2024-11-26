module Main (main) where

import SExpression
import System.Exit
import System.Environment

getFileName :: [String] -> Maybe String
getFileName [a] = Just a
getFileName _ = Nothing

checkError :: Maybe SExpr -> IO ()
checkError (Just e) = print e
checkError _ = exitWith(ExitFailure 84)

main :: IO ()
main = do
    args <- getArgs
    filename <- case getFileName args of
                Nothing -> exitWith(ExitFailure 84)
                Just filename -> pure filename
    file <- readFile filename
    checkError $ parseStringToSExpr file
