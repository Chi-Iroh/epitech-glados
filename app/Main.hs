module Main (main) where

import Debug.Trace

    -- import Debug (debug, debug2)
import AST (MainAST, isProcedureType)
import BinaryIO (writeBinary)
import Converter (convert)
import Compile (compileAST)
import Import (parseImport)
import Comment (deleteComment)
import Parser
import System.Exit (die, exitWith, ExitCode(ExitFailure))
import System.Environment
import Utils
import VM (mainVM)

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
    mainVM filename
    -- file <- readFile filename
    -- fileimport <- parseImport (deleteComment file)
    -- case fileimport of
        -- Error err -> die err
        -- -- Value content ->
            -- putResult (fmap showAll ((convert $ parse (deleteComment content)) >>= evaluateAST))

        -- Value content -> 
            -- case parse (deleteComment content) of
            --     Error err -> die err
            --     -- Value sexprs -> putResult (show sexprs)
        -- Value content -> safeToIO ((traceShowId $ convert $ parse (deleteComment content)) >>= compileAST) >>= writeBinary "output.bin"
