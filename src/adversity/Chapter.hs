module Chapter where

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe w = Just w

replaceThe :: String -> String
replaceThe s = unwords $ map f (words s) where
  f w = case notThe w of
        Nothing -> "a"
        (Just a) -> a

vowels :: String
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel ch = ch `elem` vowels

-- Write a recursive function that takes a text/string, breaks it into
-- words, and counts the number of instances of ”the” followed by a vowel-initial word.
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = rec (words s) False 0 where
  rec [] _ c = c
  rec ("the" : xs) _ c = rec xs True c
  rec ((x : _) : xs) True c = if isVowel x then rec xs False (c + 1) else rec xs False c
  rec (_ : xs) b c = rec xs b c

-- Return the number of letters that are vowels in a word.
countVowels :: String -> Int
countVowels s = length (filter isVowel s)

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = if nVowels > nConsonants
  then Nothing
  else Just (Word' s)
  where nConsonants = length s - nVowels
        nVowels = countVowels s
