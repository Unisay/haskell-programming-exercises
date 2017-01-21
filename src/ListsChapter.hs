module ListsChapter where

import Data.Char

capitalize :: String -> String
capitalize (h : t) = toUpper h : t

uppercase :: String -> String
uppercase [] = []
uppercase (h : t) = toUpper h : uppercase t

capitalHead :: String -> Char
capitalHead = toUpper . head
