module ListsChapter where

import Data.Char

capitalize :: String -> String
capitalize [] = []
capitalize (h : t) = toUpper h : t

uppercase :: String -> String
uppercase = map toUpper

capitalHead :: String -> Char
capitalHead = toUpper . head

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) = if x then x else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (== x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

-- squishMap maps a function over a list and concatenates the results
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish $ map f xs

-- squishAgain flattens a list of lists into a list. This time re-use the squishMap function.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- myMaximumBy takes a comparison function and a list and returns
-- the greatest element of the list based on the last value that the
-- comparison returned GT for.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "boom!"
myMaximumBy _ [a] = a
myMaximumBy comp (x : xs) =
  case comp x m of
    LT -> m
    GT -> x
    EQ -> x
    where m = myMaximumBy comp xs

-- myMinimumBy takes a comparison function and a list and returns
-- the lowest element of the list based on the last value that the
-- comparison returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "boom!"
myMinimumBy _ [a] = a
myMinimumBy comp (x : xs) =
  case comp x m of
    LT -> x
    GT -> m
    EQ -> x
    where m = myMinimumBy comp xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
