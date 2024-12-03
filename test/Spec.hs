import Test.HUnit
import TestParser

main :: IO Counts
main = do _ <- runTestTT tests
