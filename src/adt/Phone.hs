module Phone where

import Data.Char
import Data.List

data DaPhone = Phone [Key]

data Key = Key Digit String
         | Up Digit

type Digit = Char

myPhone :: DaPhone
myPhone = Phone [ Key '1' "1",   Key '2' "abc2", Key '3' "def3"
              , Key '4' "ghi4",  Key '5' "jkl5", Key '6' "mno6"
              , Key '7' "pqrs7", Key '8' "tuv8", Key '9' "wxyz9"
              , Key '0' "+ 0",   Key '#' ".,#" , Up  '*' ]

convo :: [String]
convo =
      [ "Wanna play 20 questions"
      , "Ya"
      , "U 1st haha"
      , "Lol ok. Have u ever tasted alcohol lol"
      , "Lol ya"
      , "Wow ur cool haha. Ur turn"
      , "Ok. Do u think I am pretty Lol"
      , "Lol ya"
      , "Haha thanks just making sure rofl ur turn"
      ]

type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps thePhone @ (Phone keys) ch
  | isUpper ch = (head [d | (Up d) <- keys], 1) : reverseTaps thePhone (toLower ch)
  | otherwise = [head [(d, snd pair) | (Key d sym) <- keys, pair <- zip sym [1..], fst pair == ch]]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead thePhone = concatMap (reverseTaps thePhone)

-- map (cellPhonesDead myPhone) convo

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

-- *Phone> map fingerTaps presses
-- [52,5,19,88,16,54,65,16,87]

-- What was the most popular letter for each message?
-- What was its cost?
-- You’ll want to combine reverseTaps and fingerTaps to figure out what it cost in taps.
mostPopularLetter :: String -> Char
mostPopularLetter = head . maximumBy (\a b -> compare (length a) (length b)) . group . sort

-- map fingerTaps $ map (reverseTaps myPhone) (map mostPopularLetter convo)
-- map fingerTaps . map (reverseTaps myPhone) . map mostPopularLetter $ convo

-- What was the most popular letter overall?
coolestLtr :: [String] -> Char
coolestLtr = minimum . map mostPopularLetter

-- What was the most popular word?
coolestWord :: [String] -> String
coolestWord sentences = head (longest (group allWords)) where
   longest = maximumBy (\a b -> length a `compare` length b)
   allWords = words (unwords sentences)
