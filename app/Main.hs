module Main (main) where

    -- import Debug (debug, debug2)
import AST (MainAST, isProcedureType)
import Converter (convert)
import Evaluate (evaluateAST)
import Import (parseImport)
import Comment (deleteComment)
import Parser
import System.Exit (die, exitWith, ExitCode(ExitFailure))
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
        Value content ->
            putResult (fmap showAll ((convert $ parse (deleteComment content)) >>= evaluateAST))

