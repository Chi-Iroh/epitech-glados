import Test.HUnit
import TestParser
import TestType
import TestConverter
import TestASTVerification

tests :: Test
tests = TestList [
    TestLabel "Parser" testParser,
    TestLabel "Type" testType,
    TestLabel "Converter" testConverter,
    TestLabel "ASTVerification" testASTVerification
    ]

main :: IO Counts
main = runTestTT tests
