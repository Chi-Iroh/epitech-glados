module TestUtils (testUtils) where

import Test.HUnit
import UtilsForTests
import Utils
import Control.Applicative ((<|>), empty)

-------------------------------------------------------------------------------

testSafeFunctorValue :: Test
testSafeFunctorValue = myAssertEqual "Safe Functor Value"
    "10"
    (show (fmap (+1) (Value 9) :: Safe Int))

testSafeFunctorError :: Test
testSafeFunctorError = myAssertEqual "Safe Functor Error"
    "\"error\""
    (show (fmap (+1) (Error "error") :: Safe Int))

testSafeApplicativeValue :: Test
testSafeApplicativeValue = myAssertEqual "Safe Applicative Value"
    "10"
    (show ((Value (+1)) <*> (Value 9) :: Safe Int))

testSafeApplicativeError :: Test
testSafeApplicativeError = myAssertEqual "Safe Applicative Error"
    "\"error\""
    (show ((Error "error") <*> (Value 9) :: Safe Int))

testSafeApplicativeErrorBoth :: Test
testSafeApplicativeErrorBoth = myAssertEqual "Safe Applicative Error Both"
    "\"error\""
    (show ((Value (+1)) <*> (Error "error") :: Safe Int))

testSafeAlternativeValue :: Test
testSafeAlternativeValue = myAssertEqual "Safe Alternative Value"
    "10"
    (show ((Value 10) <|> (Error "error") :: Safe Int))

testSafeAlternativeError :: Test
testSafeAlternativeError = myAssertEqual "Safe Alternative Error"
    "10"
    (show ((Error "error") <|> (Value 10) :: Safe Int))

testSafeAlternativeBothError :: Test
testSafeAlternativeBothError = myAssertEqual "Safe Alternative Both Error"
    "\"error\""
    (show ((Error "error") <|> (Error "other error") :: Safe Int))

testSafeMonadValue :: Test
testSafeMonadValue = myAssertEqual "Safe Monad Value"
    "10"
    (show ((Value 9) >>= (\x -> Value (x + 1)) :: Safe Int))

testSafeMonadError :: Test
testSafeMonadError = myAssertEqual "Safe Monad Error"
    "\"error\""
    (show ((Error "error") >>= (\x -> Value (x + 1)) :: Safe Int))

testSafeMonadErrorWithEmpty :: Test
testSafeMonadErrorWithEmpty = myAssertEqual "Safe Monad Error with empty"
    "\"error\""
    (show ((Error "error") >>= (\_ -> empty) :: Safe Int))

testSafeShowValue :: Test
testSafeShowValue = myAssertEqual "Safe Show Value"
    "5"
    (show ((Value 5) :: Safe Int))

testSafeShowError :: Test
testSafeShowError = myAssertEqual "Safe Show Error"
    "\"error\""
    (show ((Error "error") :: Safe Int))

testSafe :: Test
testSafe = TestList [
    TestLabel "Safe Functor Value" testSafeFunctorValue,
    TestLabel "Safe Functor Error" testSafeFunctorError,
    TestLabel "Safe Applicative Value" testSafeApplicativeValue,
    TestLabel "Safe Applicative Error" testSafeApplicativeError,
    TestLabel "Safe Applicative Error Both" testSafeApplicativeErrorBoth,
    TestLabel "Safe Alternative Value" testSafeAlternativeValue,
    TestLabel "Safe Alternative Error" testSafeAlternativeError,
    TestLabel "Safe Alternative Both Error" testSafeAlternativeBothError,
    TestLabel "Safe Monad Value" testSafeMonadValue,
    TestLabel "Safe Monad Error" testSafeMonadError,
    TestLabel "Safe Monad Error with empty" testSafeMonadErrorWithEmpty,
    TestLabel "Safe Show Value" testSafeShowValue,
    TestLabel "Safe Show Error" testSafeShowError
    ]

-------------------------------------------------------------------------------

testUtils :: Test
testUtils = TestList [
    TestLabel "Safe" testSafe
    ]
