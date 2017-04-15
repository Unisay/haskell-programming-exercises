module ApplicativeExerciseSpec where

{-

Write the Either Applicative that short-circuits on any error values.
Sum and Validation are both just alternative names for Either, but
you’ll be giving them different Applicative instances. See above for
an idea of how Validation should behave. Use the checkers library.

-}

import Data.Monoid ()
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data Sum a b = First a | Second b
  deriving (Eq, Show)

data Validation e a = Error e | Succ a
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second x) = Second $ f x
  fmap _ (First x)  = First x

instance Applicative (Sum a) where
  pure = Second
  (<*>) (Second f) (Second x) = Second $ f x
  (<*>) _          (First a)  = First a
  (<*>) (First a)  _          = First a

instance Functor (Validation e) where
  fmap f (Succ a) = Succ $ f a
  fmap _ (Error x) = Error x

instance Monoid e => Applicative (Validation e) where
  pure = Succ
  (<*>) (Succ f) (Succ a)   = Succ $ f a
  (<*>) (Succ _) (Error e)  = Error e
  (<*>) (Error e) (Succ _)  = Error e
  (<*>) (Error e) (Error r) = Error $ e `mappend` r

instance (Eq e, Eq a) => EqProp (Validation e a) where (=-=) = eq

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = frequency [ (1, fmap Error arbitrary)
                        , (1, fmap Succ arbitrary) ]

applicativeExerciseSpec :: SpecWith ()
applicativeExerciseSpec = testBatch $
  applicative (undefined :: Validation String (Int, Char, Bool))
