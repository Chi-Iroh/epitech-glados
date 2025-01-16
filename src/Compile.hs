{-# LANGUAGE TupleSections #-}

module Compile (compileAST, Symbol(..), Symbols) where

import Data.Functor ((<&>))
import Data.List (singleton, find)
import Data.Word (Word8)

import AssemblyInstructions (AssemblyInstruction(..), assemble, toAssemblyValueInstruction, toAny)
import AST (AST(..), Call(..), getType)
import Bits (u32)
import Utils (Safe(..))
import SymbolTable (SymbolTable, writeSymbolTable)
import Type
import VM (Any(..), Address)

data Symbol = BackendSymbol (String, (Symbols -> [AST] -> Safe AST))
type Symbols = [Symbol]

symbolName :: Symbol -> String
symbolName (BackendSymbol (s, _)) = s

showSymbols :: Symbols -> String
showSymbols symbols = show (map symbolName symbols)

-- traceSymbols2 :: String -> Symbols -> Symbols
-- traceSymbols2 msg f = debug2 msg (map symbolName f) f

-- traceSymbols :: Symbols -> Symbols
-- traceSymbols = traceSymbols2 "symbols: "

-- traceSymbol :: Maybe Symbol -> Maybe Symbol
-- traceSymbol f@(Just (BackendSymbol (s, _))) = debug2 "symbol: " s f
-- traceSymbol Nothing = Nothing

-- builtins :: Symbols
-- builtins = [    BackendSymbol ("*", astArithmeticOp "*" (*))
--             ,   BackendSymbol ("+", astArithmeticOp "+" (+))
--             ,   BackendSymbol ("-", astArithmeticOp "-" (-))
--             ,   BackendSymbol ("div", astArithmeticOp "div" div)
--             ,   BackendSymbol ("mod", astArithmeticOp "mod" mod)
--             ,   BackendSymbol (">", astComparisonOp ">" (>))
--             ,   BackendSymbol (">=", astComparisonOp ">=" (>=))
--             ,   BackendSymbol ("<", astComparisonOp "<" (<))
--             ,   BackendSymbol ("<=", astComparisonOp "<=" (<=))
--             ,   BackendSymbol ("eq?", astComparisonOp "eq?" (==))
--             ,   BackendSymbol ("==", astComparisonOp "==" (==))
--             ,   BackendSymbol ("if", astIf)]

updateOrAdd :: (a -> Bool) -> a -> [a] -> [a]
updateOrAdd _ a [] = [a]
updateOrAdd f a (x : xs)
    | f x = a : xs
    | otherwise = x : updateOrAdd f a xs

registerSymbol :: Symbols -> String -> (Symbols -> [AST] -> Safe AST) -> Symbols
registerSymbol symbols name f = updateOrAdd ((== name) . symbolName) (BackendSymbol (name, f)) symbols

symbolId :: String -> AST -> Symbols -> [AST] -> Safe AST
symbolId _ value _ [] = Value value
symbolId name _ _ args = Error ("Value " ++ name ++ " must be called without any argument, but you provided " ++ show args ++ " !")

fromSymbol :: AST -> Safe String
fromSymbol (ASTProcedure s) = Value s
fromSymbol arg = Error ("Lambda arguments list must only contain symbols, but got " ++ show arg ++ " !")

updateSymbols :: Symbols -> [(String, AST)] -> Symbols
updateSymbols symbols new = foldr (\(symbol, value) symbols' -> registerSymbol symbols' symbol (symbolId symbol value)) symbols new

makeLambdaSymbols :: Symbols -> [AST] -> [AST] -> Safe Symbols
makeLambdaSymbols symbols [] _ = Value symbols
makeLambdaSymbols symbols _ [] = Value symbols
makeLambdaSymbols symbols lambdaParams lambdaArgs = fmap (\params' -> updateSymbols symbols (zip params' lambdaArgs)) (mapM fromSymbol lambdaParams)

toSafe :: String -> Maybe a -> Safe a
toSafe err Nothing = Error err
toSafe _ (Just a) = Value a

findSymbol :: Symbols -> String -> Safe Symbol
findSymbol symbols symbol = toSafe ("*** ERROR : variable " ++ symbol ++ " is not bound.") (find ((== symbol) . symbolName) symbols)

data CompilationStatus = CompilationStatus {
    _instructions :: [AssemblyInstruction],
    _symbols :: [(String, CompilationStatus)],
    _nLambdas :: Int
}

emptyCompilationStatus :: CompilationStatus
emptyCompilationStatus = CompilationStatus {
    _instructions = [],
    _symbols = [],
    _nLambdas = 0
}

compileSymbols :: Int -> [(String, CompilationStatus)] -> (SymbolTable, [Word8])
compileSymbols _ [] = ([], [])
compileSymbols offset ((symbol, status) : others) = ((symbol, u32 offset) : otherTable, concatMap assemble instructions ++ otherBytes)
    where (otherTable, otherBytes) = compileSymbols (offset + length instructions) others
          instructions = _instructions status

compileAll :: CompilationStatus -> [Word8]
compileAll (CompilationStatus instructions symbols _) = concatMap assemble instructions ++ symbolInstructions
    where (symbolTable, symbolInstructions) = compileSymbols (length instructions) symbols

statusFromInstructions :: [AssemblyInstruction] -> CompilationStatus
statusFromInstructions instructions = CompilationStatus {
    _instructions = instructions,
    _symbols = [],
    _nLambdas = 0
}

(+++) :: CompilationStatus -> CompilationStatus -> Safe CompilationStatus
(+++) comp1 comp2
    | any (`elem` (map fst (_symbols comp2))) (map fst (_symbols comp1)) = Error "Duplicate symbols !"
    | otherwise = Value CompilationStatus {
        _instructions = _instructions comp1 ++ _instructions comp2,
        _symbols = symbols,
        _nLambdas = _nLambdas comp1 + _nLambdas comp2
    }
    where symbols = _symbols comp1 ++ _symbols comp2

addSymbol :: CompilationStatus -> String -> CompilationStatus -> Safe CompilationStatus
addSymbol status name status'
    | any ((== name) . fst) (_symbols status) = Error ("Symbol " ++ name ++ " already exists !")
    | otherwise = Value status {
        _symbols = (_symbols status) ++ [(name, status')]
    }

compileValue :: Show a => Type -> a -> Bool -> CompilationStatus
compileValue _type value isNested = CompilationStatus {
    _instructions = [(if isNested then PushValue else OutValue) (Any (_type, value))],
    _symbols = [],
    _nLambdas = 0
}

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = mapM f xs <&> concat

compileCall :: String -> [AST] -> Safe CompilationStatus
compileCall symbol args = concatMapM compileArg (reverse args) <&> (statusFromInstructions . (++ [Call symbol]))
    where compileArg arg = case arg of
            ASTCall (FunctionCall symbol') args' -> compileCall symbol' args' <&> _instructions
            _ -> singleton <$> toAssemblyValueInstruction PushValue arg

compileElem :: AST -> Safe [AssemblyInstruction]
compileElem a = case a of
    (ASTCall (FunctionCall name) args) -> mapM (\a' -> toAny a' <&> PushValue) (reverse args) <&> (++ [Call name])
    _ -> toAny a <&> singleton . PushValue

compileAST1 :: CompilationStatus -> AST -> Bool -> Safe CompilationStatus
compileAST1 status (ASTInt n) isNested = status +++ compileValue T_Int n isNested
compileAST1 status (ASTBool b) isNested = status +++ compileValue T_Bool b isNested
compileAST1 status (ASTCall (FunctionCall f) args) _ = compileCall f args >>= (status +++)
compileAST1 status (ASTProcedure s) _ = compileCall s [] >>= (status +++)
compileAST1 status (ASTDefine s _type ast) _ = compileAST1 emptyCompilationStatus ast True >>= addSymbol status s
compileAST1 status (ASTList []) isNested = status +++ compileValue T_EmptyList ([] :: [Int]) isNested
compileAST1 status astList@(ASTList list) isNested = getType astList >>= (\type' -> concatMapM compileElem list <&> (++ [Construct type' (length list)] ++ outputIfNotNested) >>= ((status +++) . statusFromInstructions))
    where outputIfNotNested = if isNested then [] else [Pop 0, OutRegister 0]
compileAST1 status astTuple@(ASTTuple (a, b)) isNested = getType astTuple >>= (\type' -> liftA2 (\a' b' -> b' ++ a' ++ [Construct type' 2] ++ outputIfNotNested) (compileElem a) (compileElem b)) >>= ((status +++) . statusFromInstructions)
    where outputIfNotNested = if isNested then [] else [Pop 0, OutRegister 0]
compileAST1 _ a _ = Error ("Compiling " ++ show a ++ " isn't not implemented for now !")

-- astArithmeticOp' :: String -> (Int -> Int -> Int) -> [AST] -> Safe AST
-- astArithmeticOp' _ f [(ASTInt a), (ASTInt b)] = Value $ ASTInt (f a b)
-- astArithmeticOp' name _ args = Error ("Bad arguments when attempting to call " ++ name ++ " ! Expected 2 integers but got " ++ show args ++ " !")

-- astArithmeticOp :: String -> (Int -> Int -> Int) -> Symbols -> [AST] -> Safe AST
-- astArithmeticOp name f symbols args = mapM (fst . evaluateAST1 symbols) args >>= astArithmeticOp' name f

toNumber :: AST -> Safe Int
toNumber (ASTBool b) = Value (fromEnum b)
toNumber (ASTInt n) = Value n
toNumber a = Error ("Cannot convert " ++ show a ++ " to an integer !")

-- astComparisonOp' :: String -> (Int -> Int -> Bool) -> [AST] -> Safe AST
-- astComparisonOp' name f args@[_, _] = mapM toNumber args >>= compare'
--     where compare' [a', b'] = Value $ ASTBool (f a' b')
--           compare' args' = Error ("Bad arguments when attempting to call " ++ name ++ ", can only compare booleans and integers, but got " ++ show args' ++ " !")
-- astComparisonOp' name _ args = Error ("Bad arguments when attempting to call " ++ name ++ ", can only compare 2 arguments, but got " ++ show (length args) ++ " !")

-- astComparisonOp :: String -> (Int -> Int -> Bool) -> Symbols -> [AST] -> Safe AST
-- astComparisonOp name f symbols args = mapM (fst . evaluateAST1 symbols) args >>= astComparisonOp' name f

-- astIf' :: Symbols -> [AST] -> Safe AST
-- astIf' symbols [(ASTBool condition), a, b] = if condition then eval a else eval b
--     where eval s = fst (evaluateAST1 symbols s)
-- astIf' _ args = Error ("if must be called as 'if <condition as boolean> <a> <b>', but got args " ++ show args)

-- astIf :: Symbols -> [AST] -> Safe AST
-- astIf symbols [a, b, c] = (fst $ evaluateAST1 symbols a) >>= (\a' -> astIf' symbols [a', b, c])
-- astIf _ args = Error ("if must be called with 3 arguments, but got " ++ show (length args))

compileAST' :: CompilationStatus -> [AST] -> Safe CompilationStatus
compileAST' status [] = Value status
compileAST' status (x : xs)
    | null xs = compiled
    | otherwise = compiled >>= (\status' -> compileAST' status' xs)
    where compiled = compileAST1 status x False

makeSymbolTable' :: Address -> [(String, CompilationStatus)] -> SymbolTable
makeSymbolTable' _ [] = []
makeSymbolTable' offset ((name, CompilationStatus instructions _ _) : xs) = (name, offset) : makeSymbolTable' (offset + fromIntegral (length instructions)) xs

makeSymbolTable :: [(String, CompilationStatus)] -> SymbolTable
makeSymbolTable = makeSymbolTable' 0

finishCompilation :: CompilationStatus -> [Word8]
finishCompilation (CompilationStatus instructions symbols _) = writeSymbolTable (makeSymbolTable symbols) ++ symbols' ++ concatMap assemble instructions
    where symbols' = concatMap (\sym -> concatMap assemble (_instructions (snd sym))) symbols

compileAST :: [AST] -> Safe [Word8]
compileAST ast = compileAST' emptyCompilationStatus ast <&> finishCompilation

-- test :: [SExpr]
-- test = [ List [Symbol "define", Symbol "x", List [Symbol "+", Number 6, Number 5]]
        -- , List [Symbol "eq?", Number 1, List [Symbol "mod", List [Symbol "div", List [Symbol "*", Number 5, List [Symbol "+", Number 7, List [Symbol "-", Number 10, Number 2]]], Number 5], Number 7]]
        -- , List [Symbol "if", List [Symbol ">", Symbol "x", Number 8], Symbol "#t", Symbol "#f"] ]
-- test = [ List [Symbol "define", Symbol "a", Number 5], List [List [Lambda (List [Symbol "a", Symbol "b"]) (List [Symbol "+", Symbol "a", Symbol "b"])], List [Symbol "+", Symbol "a", Symbol "a"], Symbol "a"]]
-- test = [List [Symbol "define", Symbol "x", Li /home/Chi_Iroh/.gst [Lambda (List [Symbol "a", Symbol "b"]) (List [Symbol "+", Symbol "a", Symbol "b"])]], List [Symbol "x", Number 5, Number 10]]
-- test = [List [List [Lambda (List []) (Number 15)]]]
-- test = [List [List [Lambda (List [Symbol "a"]) (List [Symbol "if", List [Symbol "eq?", Symbol "a", Number 1], Symbol "#t", Symbol "#f"])], Number 1]]
-- test = [ List [Symbol "define", Symbol "factorial", List [Lambda (List [Symbol "n"]) (List [Symbol "if", List [Symbol "<=", Symbol "n", Number 1], Number 1, List [Symbol "*", Symbol "n", List [Symbol "factorial", List [Symbol "-", Symbol "n", Number 1]]]])]], List [Symbol "factorial", Number 10]]

-- putMaybeStr :: Maybe String -> IO ()
-- putMaybeStr str = putStr $ fromMaybe "Nothing" str

-- putMaybeStrLn :: Maybe String -> IO ()
-- putMaybeStrLn str = putStrLn $ fromMaybe "Nothing" str

-- main :: IO ()
-- main = putStr $ join "\n" $ map (show . sexprToAST) test
-- main = putMaybeStrLn (fmap (\x -> join "\n" (map ((++) "Result: " . show) x)) (mapM sexprToAST (debug "SExpr: " test) >>= evaluateAST . debug "AST: "))
