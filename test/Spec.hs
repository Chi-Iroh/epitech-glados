import Test.HUnit
import TestParser
import TestType
import TestConverter

tests :: Test
tests = TestList [
    TestLabel "Parser" testParser,
    TestLabel "Type" testType,
    TestLabel "Converter" testConverter
    ]

main :: IO Counts
main = runTestTT tests
