module Builtins(builtins) where
import MathLib (mathBuiltins)
import BooleanOperator(booleanBuiltins)
import BinaryOperator(binaryBuiltins)
import DataBuiltins (Symbols)

builtins :: Symbols
builtins = mathBuiltins ++ booleanBuiltins ++ binaryBuiltins