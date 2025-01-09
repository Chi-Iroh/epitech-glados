module Builtins(builtins) where
import Evaluate(Symbols)
import MathLib (mathBuiltins)
import BooleanOperator(booleanBuiltins)

builtins :: Symbols
builtins = mathBuiltins ++ booleanBuiltins