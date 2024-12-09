module Main (main) where

import Converter
import Evaluate (evaluateAST)
import Parser
import System.Exit
import System.Environment
import Utils
import System.IO (hPutStr, stderr)

getFileName :: [String] -> Maybe String
getFileName [a] = Just a
getFileName _ = Nothing

showAll :: Show a => [a] -> String
showAll = unlines . filter (not . null) . map show

putResult :: Safe String -> IO ()
putResult (Value res) = putStr res
putResult (Error err) = hPutStr stderr ("Error: '" ++ err ++ "'")

main :: IO ()
main = do
    args <- getArgs
    filename <- case getFileName args of
                Nothing -> exitWith(ExitFailure 84)
                Just filename -> pure filename
    file <- readFile filename
    putResult (fmap showAll ((convert $ parse file) >>= evaluateAST))
    -- (convert $ parse file)-- >>= (toSafe . evaluateAST)-- >>= (print . showAll)
