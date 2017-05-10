{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MoiSpec where

import Moi
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck.Function
import Text.Show.Functions

instance Show (State s a) where
  show (State f) = "State<" ++ show f ++ ">"

instance (Arbitrary (Fun s s), Arbitrary a) => Arbitrary (State s a) where
  arbitrary = do
    a <- arbitrary
    (Fun _ f) <- arbitrary
    return $ State (\s -> (a, f s))

instance EqProp (State Int Int) where
 (=-=) (State f) (State g) = fst (f 42) `eq` fst (g 42)


stateFunctorLaws :: SpecWith ()
stateFunctorLaws = testBatch $ functor (undefined :: (State Int (Int, Int, Int)))

stateApplicativeLaws :: SpecWith ()
stateApplicativeLaws = testBatch $ applicative (undefined :: (State Int (Int, Int, Int)))

stateMonadLaws :: SpecWith ()
stateMonadLaws = testBatch $ monad (undefined :: (State Int (Int, Int, Int)))

main :: IO ()
main = hspec $ do
  stateFunctorLaws
  stateApplicativeLaws
  stateMonadLaws
