{-# LANGUAGE ExtendedDefaultRules #-}

module Gems where

import Data.Tuple (swap)

f1 :: a -> (a, a)
f1 a = (a, a)

f2 :: a -> b -> (a, b)
f2 = (,)

g1 :: (a, b) -> (b, a)
g1 = swap

dot :: (c -> d) -> (a -> b -> c) -> a -> b -> d
dot = (.).(.)

main :: IO ()
main = do
    print $ (g1 . f1) 42
    print $ (g1 `dot` f2) 21 21
