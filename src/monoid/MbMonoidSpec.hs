module MbMonoidSpec where

import Monoid
import Data.Monoid
import Test.QuickCheck
import Test.Hspec

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) r = r
  mappend l (First' Nada) = l
  mappend (First' l@(Only _)) (First' (Only _)) = First' l

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
   x <- arbitrary
   elements [(First' . Only) x, First' Nada]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = hspec $
  describe "Monoid" $ do
    it "is associative" $
      property (monoidAssoc :: FirstMappend)
    it "has left identity" $
      property (monoidLeftIdentity :: First' String -> Bool)
    it "has right identity" $
      property (monoidRightIdentity :: First' String -> Bool)
