module Jammin where

import Data.List (sortBy, groupBy)

data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)

data JamJars = Jam
  { fruit :: Fruit
  , jars :: Int }
  deriving (Eq, Show)

instance Ord JamJars where
  (<=) (Jam _ l) (Jam _ r) = l <= r

allJam :: [JamJars]
allJam = [row1, row2, row3, row4, row5, row6]
  where
  row1 = Jam Plum 1
  row2 = Jam Peach 2
  row3 = Jam Apple 42
  row4 = Jam Blackberry 0
  row5 = Jam Peach (-1)
  row6 = Jam Apple 3

totalJarsOfJam :: Int
totalJarsOfJam = sum $ fmap jars allJam

mostRow :: JamJars
mostRow = foldr (\a b -> case compare (jars a) (jars b) of EQ -> a; LT -> b; GT -> a) (head allJam) allJam

mostRow' :: JamJars
mostRow' = foldr max (head allJam) allJam

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'

sortJam :: [JamJars]
sortJam = sortBy compareKind allJam

groupJam :: [[JamJars]]
groupJam = groupBy (\l r -> fruit l == fruit r) sortJam
