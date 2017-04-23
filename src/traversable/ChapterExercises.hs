module ChapterExercises (traversableSpec) where

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Data.Monoid

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
  foldr f z (Cons h t) =  f h (foldr f z t)

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


data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y $ f z

instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z

instance Traversable (Three a b) where
  traverse f (Three x y z) = Three x y <$> f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance (Eq a, Eq b, Eq c) =>
         EqProp (Three a b c) where (=-=) = eq


data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y y') = Three' x (f y) (f y')

instance Foldable (Three' a) where
  foldMap f (Three' _ y y') = f y <> f y'

instance Traversable (Three' a) where
  traverse f (Three' x y y') = Three' x <$> f y <*> f y'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq


data S n a = S (n a) a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

instance (Applicative n, Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> (pure <$> arbitrary) <*> arbitrary

instance (Eq a, Eq (n a)) => EqProp (S n a) where (=-=) = eq


data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

{-  foldr _ z Empty = z
  foldr f z (Leaf a) = f a z
  foldr f z (Node l a r) =
    foldr f z l
    f a z
    foldr f z r-}

instance Traversable Tree where
  traverse = undefined



traversableSpec :: SpecWith ()
traversableSpec = do
  testBatch $ traversable (undefined :: Identity (Int, Int, [Int]))
  testBatch $ traversable (undefined :: Constant Int (Int, Int, [Int]))
  testBatch $ traversable (undefined :: List (Int, Int, [Int]))
  testBatch $ traversable (undefined :: Optional (Int, Int, [Int]))
  testBatch $ traversable (undefined :: Three Int Int (Int, Int, [Int]))
  testBatch $ traversable (undefined :: Three' Int (Int, Int, [Int]))
  testBatch $ traversable (undefined :: S [] (Int, Int, [Int]))

