module AsPatterns where

import Data.Char

-- This should return True if (and only if) all the values in the first
-- list appear in the second list, though they need not be contiguous.
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) haystack = elem x haystack && isSubsequenceOf xs haystack

-- Split a sentence into words, then tuple each word with the capitalized form of each.
-- Prelude> capitalizeWords "hello world"
-- [("hello", "Hello"), ("world", "World")]

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map f (words s)
  where
    f [] = ([], [])
    f w@(x : xs) = (w, toUpper x : xs)

-- Write a function that capitalizes a word.
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x : xs) = toUpper x : map toLower xs

-- Write a function that capitalizes sentences in a paragraph.
-- Recognize when a new sentence has begun by checking for periods.
capitalizeParagraph :: String -> String
capitalizeParagraph p = rec p True
  where
    rec [] _ = []
    rec (d@'.': xs) _ = d : rec xs True
    rec (w@' ': xs) b = w : rec xs b
    rec (x : xs) False = x : rec xs False
    rec (x : xs) True = toUpper x : rec xs False


-- Reuse the capitalizeWord function.
capitalizeParagraph' :: String -> String
capitalizeParagraph' p = rec $ capitalizeWord p
  where
    rec [] = []
    rec ('.' : ' ' : xs) = ". " ++ rec (capitalizeWord xs)
    rec (x : xs) = x : rec xs
