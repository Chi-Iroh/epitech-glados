import Test.HUnit
import TestParser
import TestConverter
import TestSerialize

tests :: Test
tests = TestList [
    TestLabel "Parser" testParser,
    TestLabel "Converter" testConverter,
    TestLabel "Serialize" testSerialize
    ]

main :: IO Counts
main = runTestTT tests
