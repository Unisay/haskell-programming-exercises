module ThisIsReader where

import Data.Char

{-# ANN module "HLint: ignore Use String" #-}

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  s <- cap
  b <- rev
  return (s, b)

tupledB :: [Char] -> ([Char], [Char])
tupledB = rev >>= (,) . cap

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (f . ra)

ask :: Reader a a
ask = Reader id
