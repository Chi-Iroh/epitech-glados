module Utils
    ( Safe(Value, Error),
    concatMBStrings,
    joinMBStrings
    ) where

data Safe a = Value a | Error String deriving (Eq, Ord, Read, Show)

type MBString = Maybe String

concatMBStrings :: MBString -> MBString -> MBString
concatMBStrings Nothing Nothing = Nothing
concatMBStrings Nothing other = other
concatMBStrings first Nothing = first
concatMBStrings (Just first) (Just other) = Just $ concat [first, other]

joinMBStrings :: String -> [MBString] -> MBString
joinMBStrings sep = foldr (\a b -> concatMBStrings a (concatMBStrings (b *> Just sep) b)) Nothing
