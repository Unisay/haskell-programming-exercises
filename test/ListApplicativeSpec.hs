module ListApplicativeSpec (listApplicativeSpec) where

import ListApplicative
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fmap fromList arbitrary where
    fromList :: [a] -> List a
    fromList = foldr Cons Nil

instance Eq a => EqProp (List a) where (=-=) = eq

listApplicativeSpec :: SpecWith ()
listApplicativeSpec = testBatch $ applicative (undefined :: List (Int, Char, Bool))
