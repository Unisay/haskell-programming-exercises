module Instances (traversableSpec) where

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq


newtype Constant a b = Constant a
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure $ Constant x

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where (=-=) = eq


data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons h t) = Cons (f h) $ fmap f t

instance Foldable List where
  foldr _ z Nil = z
  foldr f z (Cons h t) = foldr f (f h z) t

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons h t) = Cons <$> f h <*> traverse f t

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fromList <$> arbitrary where
    fromList :: [a] -> List a
    fromList = foldr Cons Nil

instance Eq a => EqProp (List a) where (=-=) = eq


data Optional a = Nada | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [pure Nada, Yep <$> arbitrary]

instance Eq a => EqProp (Optional a) where (=-=) = eq


traversableSpec :: SpecWith ()
traversableSpec = do
  testBatch $ traversable (undefined :: Identity (Int, Bool, String))
  testBatch $ traversable (undefined :: Constant Int (Int, Bool, String))
  testBatch $ traversable (undefined :: List (Int, Bool, String))
  testBatch $ traversable (undefined :: Optional (Int, Bool, String))
