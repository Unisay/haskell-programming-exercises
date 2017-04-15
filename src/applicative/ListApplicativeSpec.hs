module ListApplicativeSpec (listApplicativeSpec) where

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Data.Semigroup

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Semigroup (List a) where
  l <> Nil = l
  Nil <> r = r
  (Cons x xs) <> ys = Cons x (xs <> ys)

instance Applicative List where
  pure a = Cons a Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons f ft) <*> as = fmap f as <> (ft <*> as)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fmap fromList arbitrary where
    fromList :: [a] -> List a
    fromList = foldr Cons Nil

instance Eq a => EqProp (List a) where (=-=) = eq

listApplicativeSpec :: SpecWith ()
listApplicativeSpec = testBatch $ applicative (undefined :: List (Int, Char, Bool))
