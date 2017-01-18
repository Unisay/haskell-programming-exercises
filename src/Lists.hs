module Lists where

{-
Write your own enumFromTo definitions for the types provided. Do not
use range syntax to do so. It should return the same results as if you
did [start..stop]
-}

eftBool :: Bool -> Bool -> [Bool]
eftBool = eftEnum

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eftEnum

eftInt :: Int -> Int -> [Int]
eftInt = eftEnum

eftChar :: Char -> Char -> [Char]
eftChar = eftEnum

eftEnum :: (Enum a, Ord a) => a -> a -> [a]
eftEnum from to
  | from < to = from : (eftEnum (succ from) to)
  | from == to = [from]
  | otherwise = []
