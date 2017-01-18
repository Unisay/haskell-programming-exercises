module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eigh t"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits n = recur n [] where
  recur num acc
    | init < 10 = init : last : acc
    | otherwise = recur init (last : acc)
      where
        init = div num 10
        last = mod num 10

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" (map digitToWord (digits n))
