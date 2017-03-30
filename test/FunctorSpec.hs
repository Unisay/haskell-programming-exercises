module FunctorSpec (functorSpec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

{-# ANN module "HLint: ignore Functor law" #-}

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = fmap (g . f) x == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

functorSpec :: SpecWith ()
functorSpec = describe "Functor laws" $ do
  it "has identity" $ property (functorIdentity :: [Int] -> Bool)
  it "is composable" $ property (functorCompose :: [Int] -> IntToInt -> IntToInt -> Bool)
