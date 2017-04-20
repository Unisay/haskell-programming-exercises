module BasicDerivedOpsExercises where

{-
   Implement the functions in terms of foldMap or foldr from Foldable,
   then try them out with multiple types that have Foldable instances.
-}

import Prelude hiding (sum, foldr, foldMap, minimum, maximum, null)
import Data.Monoid
import Data.Foldable (foldr, foldMap)

sum :: (Foldable t, Num a) => t a -> a
sum t = getSum $ foldMap Sum t

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a = foldr ((||) . (== a)) False  

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr f Nothing where
  f a Nothing = Just a
  f a (Just b) = Just $ min a b

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr f Nothing where
  f a Nothing = Just a
  f a (Just b) = Just $ max a b

null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

length :: (Foldable t) => t a -> Int
length = foldr (\_ b -> b + 1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty