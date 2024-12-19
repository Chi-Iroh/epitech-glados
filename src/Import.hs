module Import (parseImport) where

import Utils (Safe(..))
import Control.Exception (try, IOException)
import Data.List (isPrefixOf)

-- Take a list of words as input (from a line) and open file if correct
processLine :: [String] -> IO (Safe String)
processLine [_, filepath] = do
    result <- try (readFile filepath) :: IO (Either IOException String)
    return $ case result of
        Left _      -> Error ("GLaDOS: ImportError: error importing file \"" ++ filepath ++ "\". Make sure file exist.")
        Right content -> Value content
processLine _ = return $ Error "GLaDOS: ImportError: Invalid import line format."

-- Process lines and handle imports
catchImport :: [String] -> IO (Safe String)
catchImport [] = return $ Value ""
catchImport (line:rest)
    | "import" `isPrefixOf` line =
        case words line of
            [_import, filepath] -> do
                importResult <- processLine (words line)
                case importResult of
                    Error err -> return $ Error err
                    Value importContent -> do
                        restResult <- catchImport rest
                        case restResult of
                            Error err -> return $ Error err
                            Value restContent -> return $ Value (importContent ++ "\n" ++ restContent)
            _ -> return $ Error "GLaDOS: ImportError: Import line must contain only one file path."
    | otherwise = do
        restResult <- catchImport rest
        case restResult of
            Error err -> return $ Error err
            Value restContent -> return $ Value (line ++ "\n" ++ restContent)

-- Parse the input string and handle imports
parseImport :: String -> IO (Safe String)
parseImport input = catchImport (lines input)
