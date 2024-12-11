import Test.HUnit
import TestParser
import TestConverter

tests :: Test
tests = TestList [
    TestLabel "Parser" testParser,
    TestLabel "Converter" testConverter
    ]

main :: IO Counts
main = runTestTT tests
