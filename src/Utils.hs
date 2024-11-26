module Utils
    ( concatMBStrings,
    joinMBStrings
    ) where

type MBString = Maybe String

concatMBStrings :: MBString -> MBString -> MBString
concatMBStrings Nothing Nothing = Nothing
concatMBStrings Nothing other = other
concatMBStrings first Nothing = first
concatMBStrings (Just first) (Just other) = Just $ concat [first, other]

joinMBStrings :: String -> [MBString] -> MBString
joinMBStrings sep = foldr (\a b -> concatMBStrings a (concatMBStrings (b *> Just sep) b)) Nothing
