module Main where

import           Test.Hspec
import           RecursionSpec   (recursionSpec)
import           WordNumberSpec  (wordNumberSpec)

main :: IO ()
main = hspec $ do
  wordNumberSpec
  recursionSpec
