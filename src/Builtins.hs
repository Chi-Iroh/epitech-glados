module Builtins(builtins) where
import Evaluate(Symbols)
import AST (AST(..), Call(..), MainAST(..))
import Data.List (singleton)
import Utils (Safe(..))
import Type
import MathLib (mathBuiltins)
import BooleanOperator(booleanBuiltins)

builtins :: Symbols
builtins = mathBuiltins ++ booleanBuiltins