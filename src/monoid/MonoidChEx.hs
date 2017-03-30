module MonoidChEx where

import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)
newtype Identity a = Identity a deriving Show
data Two a b = Two a b deriving Show

instance Semigroup Trivial where
  _ <> _  = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a

instance Arbitrary Trivial where
  arbitrary = return Trivial

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

