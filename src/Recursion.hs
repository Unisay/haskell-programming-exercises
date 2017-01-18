module Recursion where

{-
Write a function that recursively sums all numbers from 1 to n,
n being the argument. So that if n was 5, you’d add 1 + 2 + 3 + 4 +
5 to get 15. The type should be (Eq a, Num a) => a -> a.
-}

sumit :: (Ord n, Num n) => n -> n
sumit n = go n 0
  where go num acc
         | num <= 0 = acc
         | otherwise = go (num - 1) (acc + num)

{-
Write a function that multiplies two integral numbers using
recursive summation. The type should be (Integral a) => a ->
a -> a.
-}

mulit :: (Integral a) => a -> a -> a
mulit x y = go x y 0 where
  go a n acc
    | n == 0 = acc
    | otherwise = go a (n - 1) (acc + a)

mc91 :: (Num a, Ord a) => a -> a
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91 . mc91 $ (n + 11)
