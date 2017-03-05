module Main where

import Test.Hspec
import Recursion

main :: IO ()
main = hspec $
  describe "mulit" $ do
    it "2 mulit 3 == 6" $
      2 `mulit` 3 `shouldBe` (6 :: Int)
    it "0 mulit 42 == 0" $
      0 `mulit` 42 `shouldBe` (0 :: Int)
    it "42 mulit 0 == 0" $
      42 `mulit` 0 `shouldBe` (0 :: Int)
