module Builtins(builtins) where
import Evaluate(Symbols)
import MathLib (mathBuiltins)
import BooleanOperator(booleanBuiltins)
import BinaryOperator(binaryBuiltins)

builtins :: Symbols
builtins = mathBuiltins ++ booleanBuiltins ++ binaryBuiltins