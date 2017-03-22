module WordNumber where

import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "digitToWord is not a total function"

digits :: Int -> [Int]
digits n = dropWhile (==0) $ recur n [] where
  recur num acc
    | dv < 10 = dv : md : acc
    | otherwise = recur dv (md : acc)
      where
        dv = div num 10
        md = mod num 10

wordNumber :: Int -> String
wordNumber n = intercalate "-" (map digitToWord (digits n))
