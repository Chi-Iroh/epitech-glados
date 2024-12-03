import Test.HUnit
import TestParser

main :: IO Counts
main = runTestTT testParser
