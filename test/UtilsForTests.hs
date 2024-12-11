module UtilsForTests (
        red,
        yellow,
        green,
        blank,
        colorize,
        myAssertEqual
    ) where

import Test.HUnit

red :: String
red = "\x1b[31m"

yellow :: String
yellow = "\x1b[1;33m"

green :: String
green = "\x1b[0;32m"

blank :: String
blank = "\x1b[0m"

colorize :: String -> String -> String
colorize color message = color ++ message ++ blank

-------------------------------------------------------------------------------

myAssertEqual :: Eq a => Show a => String -> a -> a -> Test
myAssertEqual title expected actual = TestCase (assertEqual (colorize red title) expected actual)
