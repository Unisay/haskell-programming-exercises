module ApplicativeChapterSpec where

import Data.Monoid ()
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

{-
instance Applicative [] where
  pure :: a -> [a]
  pure a = [a]

  (<*>) :: [a -> b] -> [a] -> [b]
  (<*>) = zipWith ($)

instance Applicative IO where
  pure :: a -> IO a
  pure = return

  (<*>) :: IO (a -> b) -> IO a -> IO b
  (<*>) iof ioa = do
    f <- iof
    a <- ioa
    return (f a)

instance Applicative ((,) a) where
  pure :: a -> (,) a a
  pure a = (,) a a

  (<*>) :: (a, a -> b) -> (a, a) -> (a, b)
  (<*>) (a, f) (_, x) = (a, f x)

instance Applicative (a -> e) where
  pure :: a -> (Int -> a)
  pure a _ = a

  (<*>) :: (Int -> (a -> b)) -> (Int -> a) -> (Int -> b)
  (ab <*> a) i = ab i (a i)
-}

newtype Identity a = Identity a
  deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity l) <*> (Identity r) = Identity (l r)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq


data Pair a = Pair a a
  deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (Pair x x') <*> (Pair y y') = Pair (x y) (x' y')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    return $ Pair a1 a2

instance Eq a => EqProp (Pair a) where
  (=-=) = eq


data Two a b = Two a b
  deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (Two x y) <*> (Two x' y') = Two (x `mappend` x') (y y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq


data Three a b c = Three a b c
  deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three j k f) <*> (Three x y z) = Three (j `mappend` x) (k `mappend` y) (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq


data Three' a b = Three' a b b
  deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' x y y') = Three' x (f y) (f y')

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (Three' a f f') <*> (Three' x y y') = Three' (a `mappend` x) (f y) (f' y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq


data Four a b c d = Four a b c d
  deriving (Show, Eq)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (Four j k f g) <*> (Four x y z e) =
    Four (j `mappend` x) (k `mappend` y) (f `mappend` z) (g e)


data Four' a b = Four' a a a b
  deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' q w e r) <*> (Four' t y u i) =
    Four' (q `mappend` t) (w `mappend` y) (e `mappend` u) (r i)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    b0 <- arbitrary
    return $ Four' a1 a2 a3 b0

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq


applicativeChapterSpec :: SpecWith ()
applicativeChapterSpec = do
  testBatch $ applicative (undefined :: Identity (Int, Char, Bool))
  testBatch $ applicative (undefined :: Pair (Int, Char, Bool))
  testBatch $ applicative (undefined :: Two String (Int, Char, Bool))
  testBatch $ applicative (undefined :: Three String String (Int, Char, Bool))
  testBatch $ applicative (undefined :: Three' String (Int, Char, Bool))
  testBatch $ applicative (undefined :: Four' String (Int, Char, Bool))
