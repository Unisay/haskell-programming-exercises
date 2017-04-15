module MonadSpec where

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers


data Sum a b = First a | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second (f b)
  fmap _ (First a) = First a

instance Applicative (Sum a) where
  pure = Second
  (<*>) (Second f) (Second x) = Second (f x)
  (<*>) (First x)   _         = First x
  (<*>) _          (First x)  = First x

instance Monad (Sum a) where
  return = pure
  (>>=) (First x)  _ = First x
  (>>=) (Second x) f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
 arbitrary = frequency [ (1, fmap First arbitrary)
                       , (1, fmap Second arbitrary)
                       ]

instance (Eq a, Eq b) => EqProp (Sum a b) where
 (=-=) = eq

monadSpec :: SpecWith ()
monadSpec = testBatch $ monad (undefined :: (Sum Int (Int, Bool, Char)))
