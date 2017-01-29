module Scans where

fibs :: [Int]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Int
fibsN x = fibs !! x

-- Modify your fibs function to only return the first 20 Fibonacci numbers.
fibs20 :: [Int]
fibs20 = 1 : take 19 (scanl (+) 1 fibs20)

-- Modify fibs to return the Fibonacci numbers that are less than 100.
fibs100 :: [Int]
fibs100 = 1 : takeWhile (< 100) (scanl (+) 1 fibs100)

-- Try to write the factorial function from Recursion as a scan.
-- You’ll want scanl again, and your start value will be 1.
-- Warning: this will also generate an infinite list, so you may want to pass it through a take function or similar.
factorial :: Int -> [Int]
factorial i = scanl (*) 1 [1 .. i]

factorialN :: Int -> Int
factorialN = last . factorial
