import Test.HUnit
import TestParser
import TestType
import TestConverter
import TestASTVerification
import TestSymbolTable
import TestSerialize

tests :: Test
tests = TestList [
    TestLabel "Parser" testParser,
    TestLabel "Type" testType,
    TestLabel "Converter" testConverter,
    TestLabel "ASTVerification" testASTVerification,
    TestLabel "SymbolTable" testSymbolTable,
    TestLabel "Serialize" testSerialize
    ]

main :: IO Counts
main = runTestTT tests
