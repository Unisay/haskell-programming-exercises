module ListApplicative where

import Data.Semigroup

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Semigroup (List a) where
  l <> Nil = l
  Nil <> r = r
  (Cons x xs) <> ys = Cons x (xs <> ys)

instance Applicative List where
  pure a = Cons a Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons f ft) <*> as = fmap f as <> (ft <*> as)

