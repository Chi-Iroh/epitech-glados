import Test.HUnit
import TestParser
import TestConverter
import TestSymbolTable

tests :: Test
tests = TestList [
    TestLabel "Parser" testParser,
    TestLabel "Converter" testConverter,
    TestLabel "SymbolTable" testSymbolTable
    ]

main :: IO Counts
main = runTestTT tests
