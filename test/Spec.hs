import Test.HUnit
import TestUtils
import TestSExpression
import TestParser
import TestType
import TestAST
import TestConverter
import TestASTVerification
import TestSymbolTable
import TestSerialize

tests :: Test
tests = TestList [
    TestLabel "Utils" testUtils,
    TestLabel "SExpression" testSExpression,
    TestLabel "Parser" testParser,
    TestLabel "Type" testType,
    TestLabel "AST" testAST,
    TestLabel "Converter" testConverter,
    TestLabel "ASTVerification" testASTVerification,
    TestLabel "SymbolTable" testSymbolTable,
    TestLabel "Serialize" testSerialize
    ]

main :: IO Counts
main = runTestTT tests
