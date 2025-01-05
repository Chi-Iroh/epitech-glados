{-# LANGUAGE NumericUnderscores #-}
module IntLimits where

uintMax :: Int
uintMax = 4_294_967_295

intMin :: Int
intMin = -2_147_483_648

intMax :: Int
intMax = 2_147_483_647

isInRange :: Ord a => a -> a -> a -> Bool
isInRange low high val
    | val < low = False
    | val > high = False
    | otherwise = True

checkInt :: Int -> Bool
checkInt int = isInRange intMin intMax int

checkUInt :: Int -> Bool
checkUInt uint = isInRange 0 uintMax uint