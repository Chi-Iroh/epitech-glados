module TestParser (tests) where

import Test.HUnit
import Utils
import SExpression
import Parser

test1 :: Test
test1 = TestCase (assertEqual "parse '0'" (Value $ SList [(SNumber 0)]) (parse "0"))

test2 :: Test
test2 = TestCase (assertEqual "parse ')'" (Error "GLaDOS: SyntaxError: unexpected ')' while parsing") (parse ")"))

tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]
