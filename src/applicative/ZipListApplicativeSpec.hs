module ZipListApplicativeSpec (zipListApplicativeSpec) where

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import ListApplicativeSpec

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n l@(Cons h t)
  | n < 0 = l
  | otherwise = Cons h (take' (n - 1) t)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = ZipList' . pure
  (ZipList' Nil) <*> _ = ZipList' Nil
  _ <*> (ZipList' Nil) = ZipList' Nil
  (ZipList' (Cons f ft)) <*> (ZipList' (Cons h t)) = ZipList' $
    let (ZipList' rest) = ZipList' ft <*> ZipList' t
    in Cons (f h) rest

-- > ZipList' (Cons (+1) (Cons (+2) Nil)) <*> ZipList' (Cons 10 (Cons 20 (Cons 30 Nil)))
-- ZipList' (Cons 11 (Cons 22 Nil))

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = fmap ZipList' arbitrary

zipListApplicativeSpec :: SpecWith ()
zipListApplicativeSpec = testBatch $ applicative (undefined :: List (Int, Char, Bool))
