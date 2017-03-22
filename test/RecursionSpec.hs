module RecursionSpec (recursionSpec) where

import           Test.Hspec
import           Test.QuickCheck
import           Recursion

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x


recursionSpec :: SpecWith ()
recursionSpec = describe "mulit" $
  do
    it "2 mulit 3 == 6" $
      2 `mulit` 3 `shouldBe` (6 :: Int)
    it "0 mulit 42 == 0" $
      0 `mulit` 42 `shouldBe` (0 :: Int)
    it "42 mulit 0 == 0" $
      42 `mulit` 0 `shouldBe` (0 :: Int)
    it "x + 1 should always be greater than x" $
      property prop_additionGreater
