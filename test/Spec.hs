import Test.HUnit
import TestParser
import TestConverter
import TestSymbolTable
import TestSerialize

tests :: Test
tests = TestList [
    TestLabel "Parser" testParser,
    TestLabel "Converter" testConverter,
    TestLabel "SymbolTable" testSymbolTable,
    TestLabel "Serialize" testSerialize
    ]

main :: IO Counts
main = runTestTT tests
