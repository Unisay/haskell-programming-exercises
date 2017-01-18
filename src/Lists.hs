module Lists where

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
