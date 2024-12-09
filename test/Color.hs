module Color (
        red,
        yellow,
        green,
        blank,
        colorize
    ) where

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
