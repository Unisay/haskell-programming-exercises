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

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary


data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    return $ Pair a1 a2

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a' x) = Two a' (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a' b c) = Three' a' (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Three' a b b

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z q) = Four x y z (f q)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = do
      (Three a b c) <- arbitrary
      d <- arbitrary
      return $ Four a b c d

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return $ Four' a a a b

functorSpec :: SpecWith ()
functorSpec = do

  describe "Functor laws" $ do
    it "has identity" $
      property (functorIdentity :: [Int] -> Bool)
    it "is composable" $
      property (functorCompose :: [Int] -> IntToInt -> IntToInt -> Bool)

  describe "Functor Identity" $ do
    it "has identity" $
      property (functorIdentity :: Identity Int -> Bool)
    it "is composable" $
      property (functorCompose :: Identity Int -> IntToInt -> IntToInt -> Bool)

  describe "Functor Pair" $ do
    it "has identity" $
      property (functorIdentity :: Pair Int -> Bool)
    it "is composable" $
      property (functorCompose :: Pair Int -> IntToInt -> IntToInt -> Bool)

  describe "Functor Three" $ do
    it "has identity" $
      property (functorIdentity :: Three Bool Char Int -> Bool)
    it "is composable" $
      property (functorCompose :: Three Bool Char Int -> IntToInt -> IntToInt -> Bool)

  describe "Functor Three'" $ do
    it "has identity" $
      property (functorIdentity :: Three' Char Int -> Bool)
    it "is composable" $
      property (functorCompose :: Three' Char Int -> IntToInt -> IntToInt -> Bool)

  describe "Functor Four" $ do
    it "has identity" $
      property (functorIdentity :: Four Char Char Char Int -> Bool)
    it "is composable" $
      property (functorCompose :: Four Char Char Char Int -> IntToInt -> IntToInt -> Bool)

  describe "Functor Four'" $ do
    it "has identity" $
      property (functorIdentity :: Four' Char Int -> Bool)
    it "is composable" $
      property (functorCompose :: Four' Char Int -> IntToInt -> IntToInt -> Bool)
