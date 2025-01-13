module Main (main) where

import Debug.Trace

    -- import Debug (debug, debug2)
import AST (MainAST, isProcedureType)
import BinaryIO (readBinary, writeBinary)
import Converter (convert)
import Compile (compileAST)
import Data.Functor ((<&>))
import Import (parseImport)
import Comment (deleteComment)
import Parser
import System.Exit (die, exitWith, ExitCode(ExitFailure, ExitSuccess))
import System.Environment
import Utils

getFileName :: [String] -> Maybe String
getFileName [a] = Just a
getFileName _ = Nothing

showAll' :: Show a => [a] -> String
showAll' = unlines . filter (not . null) . map show

showAll :: [MainAST] -> String
showAll [] = ""
showAll args
    | length args == 1 && isProcedureType (head args) = "#\\<procedure\\>"
    | otherwise = showAll' args

putResult :: Safe String -> IO ()
putResult (Value res) = putStr res
putResult (Error err) = die err

safeToIO :: Safe a -> IO a
safeToIO (Error err) = die err
safeToIO (Value a) = pure a

main :: IO ()
main = do
    args <- getArgs
    filename <- case getFileName args of
                Nothing -> exitWith(ExitFailure 84)
                Just filename -> pure filename
    file <- readFile filename
    fileimport <- parseImport (deleteComment file)
    case fileimport of
        Error err -> die err
        -- Value content -> putStrLn (deleteComment content)
        Value content -> safeToIO ((traceShowId $ convert $ parse (deleteComment content)) >>= compileAST) >>= writeBinary "output.bin"
