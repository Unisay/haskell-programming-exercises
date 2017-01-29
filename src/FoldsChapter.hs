module FoldsChapter where
-- 1. Given the following sets of consonants and vowels:
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

{-
a) Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible
   stop-vowel-stop combinations. These will not all correspond to real words in English,
   although the stop-vowel-stop pattern is common enough that many of them will.
-}
svs :: [(Char, Char, Char)]
svs = [ (a, b, c) | a <- stops, b <- vowels, c <- stops ]

-- b) Modify that function so that it only returns the combinations that begin with a p.
psvs :: [(Char, Char, Char)]
psvs = [ t | t @ ('p', _, _) <- svs ]

{-
c) Now set up lists of nouns and verbs (instead of stops and vowels) and modify the function
   to make tuples representing possible noun-verb-noun sentences.
-}
nouns :: [String]
nouns = [ "car", "man", "earth", "bar", "steam" ]

verbs :: [String]
verbs = [ "drives", "makes", "sees", "eats", "goes", "does" ]

abc :: [a] -> [a] -> [a] -> [(a, a, a)]
abc as bs cs = [ (a, b, c) | a <- as, b <- bs, c <- cs ]

svs' :: [(Char, Char, Char)]
svs' = abc stops vowels stops

nvn :: [(String, String, String)]
nvn = abc nouns verbs nouns

seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))

avgWordLen :: String -> Double
avgWordLen x = totalLength / numWords where
  totalLength = fromIntegral $ sum (map length ws)
  numWords = fromIntegral $ length ws
  ws = words x

-- myOr returns True if any Bool in the list is True.
myOr :: [Bool] -> Bool
myOr = foldr (||) False

myOr2 :: [Bool] -> Bool
myOr2 [] = False
myOr2 (True : _) = True
myOr2 (_ : xs) = myOr2 xs

-- direct recursion, using (||)
myOr3 :: [Bool] -> Bool
myOr3 [] = False
myOr3 (x : xs) = x || myOr3 xs

-- myAny returns True if a -> Bool applied to any of the values in the list returns True.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = myOr . map f

-- In addition to the recursive and fold based myElem, write a version that uses any.
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x: xs) = (x == e) || myElem e xs

myElemF :: Eq a => a -> [a] -> Bool
myElemF e = foldr ((||) . (== e)) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' a = any (== a)

-- Implement myReverse, don’t worry about trying to make it lazy.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- Write myMap in terms of foldr. It should have the same behavior as the built-in map.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- Write myFilter in terms of foldr. It should have the same behavior as the built-in filter.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\e t -> if p e then e : t else t) []

-- squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish = foldr (++) []

-- squishMap maps a function over a list and concatenates the results.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []
-- squishMap f =  squish . map f

-- squishAgain flattens a list of lists into a list.
-- This time re-use the squishMap function.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- myMaximumBy takes a comparison function and a list and returns
-- the greatest element of the list based on the last value that the comparison returned GT for.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f as = foldr (\a b -> case f a b of LT -> b; _ -> a) (head as) as

-- myMinimumBy takes a comparison function and a list and returns
-- the least element of the list based on the last value that the comparison returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = myMaximumBy (flip f)
