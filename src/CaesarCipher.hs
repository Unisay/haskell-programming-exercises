module CaesarCipher where

import Data.Char

shiftBounded :: Int -> Int -> Int -> Char -> Char
shiftBounded lower upper n c
  | pos < lower = chr $ span + pos + 1
  | pos > upper = chr $ pos - span - 1
  | otherwise = chr pos
  where
    pos = mod n span + ord c
    span = upper - lower

shift :: Int -> Char -> Char
shift n c
  | isUpper c = shiftBounded (ord 'A') (ord 'Z') n c
  | otherwise = shiftBounded (ord 'a') (ord 'z') n c

unShift :: Int -> Char -> Char
unShift n = shift (1 - n)

caesar :: Int -> String -> String
caesar n s = map (shift n) s

unCaesar :: Int -> String -> String
unCaesar n s = map (unShift n) s

