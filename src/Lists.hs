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

eftChar :: Char -> Char -> String
eftChar = eftEnum

eftEnum :: (Enum a, Ord a) => a -> a -> [a]
eftEnum from to
  | from < to = from : eftEnum (succ from) to
  | from == to = [from]
  | otherwise = []

{-
Using takeWhile and dropWhile, write a function that takes a string
and returns a list of strings, using spaces to separate the elements
of the string into words, as in the following sample:
*Main> myWords "all i wanna do is have some fun"
["all","i","wanna","do","is","have","some","fun"]
-}

breakWords :: Char -> String -> [String]
breakWords _ "" = []
breakWords c s  = h : breakWords c t
  where
    notSeparator = (/= c)
    h = takeWhile notSeparator s
    t = drop 1 (dropWhile notSeparator s)

myWords :: String -> [String]
myWords = breakWords ' '
