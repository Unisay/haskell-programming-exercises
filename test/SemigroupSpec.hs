 {-# OPTIONS_GHC -fno-warn-orphans #-}

module SemigroupSpec where

import Semigroup
import Data.Semigroup
import Test.Hspec
import Test.QuickCheck

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance (Arbitrary t, Arbitrary i) => Arbitrary (Two t i) where
  arbitrary = do
    t' <- arbitrary
    i' <- arbitrary
    return $ Two t' i'

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    (Two a b) <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = do
      (Three a b c) <- arbitrary
      d <- arbitrary
      return $ Four a b c d

instance Arbitrary BoolConj where
  arbitrary = fmap BoolConj arbitrary

instance Arbitrary BoolDisj where
  arbitrary = fmap BoolDisj arbitrary

instance (Arbitrary t, Arbitrary i) => Arbitrary (Or t i) where
  arbitrary = oneof [ fmap Fst arbitrary, fmap Snd arbitrary ]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type I = Identity Trivial
type T = Two Trivial I
type Th = Three Trivial I T
type F = Four Trivial I T Th
type O = Or I T

main :: IO ()
main = hspec $
  describe "Is associative" $ do
    it "Semigroup Trivial" $
      property (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
    it "Semigroup (Identity Trivial)" $
      property (semigroupAssoc :: I -> I -> I -> Bool)
    it "Semigroup (Two a b)" $
      property (semigroupAssoc :: T -> T -> T -> Bool)
    it "Semigroup (Three a b c)" $
      property (semigroupAssoc :: Th -> Th -> Th -> Bool)
    it "Semigroup (Four a b c d)" $
      property (semigroupAssoc :: F -> F -> F -> Bool)
    it "Semigroup BoolConj" $
      property (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
    it "Semigroup BoolDisj" $
      property (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
    it "Semigroup (Or a b)" $
      property (semigroupAssoc :: O -> O -> O -> Bool)
