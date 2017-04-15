module ChapterExercises where

import Prelude (Eq, Show, Int, Bool, Char, ($), undefined, const, foldr, id, flip)
import Control.Applicative
import Data.Functor
import Data.Semigroup
import Control.Monad
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data Nope a = NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure = const NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = elements [ NopeDotJpg ]

instance EqProp (Nope a) where (=-=) = eq

nopeMonadSpec :: SpecWith ()
nopeMonadSpec = do
  let nopeT = undefined :: Nope (Int, Bool, Char)
  testBatch $ functor nopeT
  testBatch $ applicative nopeT
  testBatch $ monad nopeT


data PhhhbbtttEither b a = Left a | Right b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left x) = Left $ f x
  fmap _ (Right x) = Right x

instance Applicative (PhhhbbtttEither b) where
  pure = Left
  (<*>) (Left f) (Left x) = Left $ f x
  (<*>) (Right f) _ = Right f
  (<*>) _ (Right f) = Right f

instance Monad (PhhhbbtttEither b) where
 (>>=) (Left x) f = f x
 (>>=) (Right x) _ = Right x

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (PhhhbbtttEither b a) where
    arbitrary = oneof [ fmap Right arbitrary, fmap Left arbitrary ]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

phhhbbtttEitherSpec :: SpecWith ()
phhhbbtttEitherSpec = do
  let phbT = undefined :: PhhhbbtttEither Int (Int, Bool, Char)
  testBatch $ functor phbT
  testBatch $ applicative phbT
  testBatch $ monad phbT


newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity $ f x

instance Monad Identity where
  return = pure
  (>>=) (Identity x) f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

identitySpec :: SpecWith ()
identitySpec = do
  let identityT = undefined :: Identity (Int, Bool, Char)
  testBatch $ functor identityT
  testBatch $ applicative identityT
  testBatch $ monad identityT


data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Semigroup (List a) where
  l <> Nil = l
  Nil <> r = r
  (Cons x xs) <> ys = Cons x (xs <> ys)

instance Applicative List where
  pure x = Cons x Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons f ft) <*> as = fmap f as <> (ft <*> as)

instance Monad List where
  (>>=) Nil _ = Nil
  (>>=) (Cons x t) f = f x <> (t >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fmap fromList arbitrary where
    fromList :: [a] -> List a
    fromList = foldr Cons Nil

instance Eq a => EqProp (List a) where (=-=) = eq

listSpec :: SpecWith ()
listSpec = do
  let listT = undefined :: List (Int, Char, Bool)
  testBatch $ functor listT
  testBatch $ applicative listT
  testBatch $ monad listT


j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a ma mab = mab >>= flip fmap ma

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = liftM2 (:) (f x) (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id
