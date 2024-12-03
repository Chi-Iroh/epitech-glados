module Main (main) where

import Converter
import Parser
import System.Exit
import System.Environment
import Utils

getFileName :: [String] -> Maybe String
getFileName [a] = Just a
getFileName _ = Nothing

main :: IO ()
main = do
    args <- getArgs
    filename <- case getFileName args of
                Nothing -> exitWith(ExitFailure 84)
                Just filename -> pure filename
    file <- readFile filename
    printSafeList $ convert $ parse file
