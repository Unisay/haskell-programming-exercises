{-# LANGUAGE TypeOperators, KindSignatures, RankNTypes, ExistentialQuantification #-}

module Arrows where

import Data.Int
import qualified GHC.Base (id, (.))
import qualified Prelude hiding (Monoid)
import Data.Functor

class Category cat where
  id  :: a `cat` a
  (.) :: cat b c -> cat a b -> cat a c

instance Category (->) where
  id  = GHC.Base.id
  (.) = (GHC.Base..)

class Monoid m where
  mu  :: (m, m) -> m
  eta :: ()     -> m

newtype IntFun = IF (Int -> Int)

instance Monoid IntFun where
  mu (IF f, IF g) = IF (g . f)
  eta _ = IF id

class BiFunctor  (b :: * -> * -> *) where
  bimap :: (x -> x') -> (y -> y') -> b x y -> b x' y'

class Profunctor (p :: * -> * -> *) where
  dimap :: (x' -> x) -> (y -> y') -> p x y -> p x' y'

instance Profunctor (->) where
  dimap con pro f = pro . f . con

-- Profunctor p =>
type End p = forall x. p x x
-- type Coend p = exists x. p x x
data Coend p = forall x. Coend (p x x)

newtype NatPro f g a b = NatPro (f a -> g b)

instance (Functor f, Functor g) => Profunctor (NatPro f g) where
  dimap ba cd (NatPro p) = NatPro (fmap cd . p . fmap ba)

type Nat f g = End (NatPro f g)
-- type Nat f g = forall x. f x -> g x

-- (exists x. C x) -> y ~ forall x. C x -> y

-- (Profunctor p, Profunctor q) =>
-- type Compose p q a b = exists x. (p a x, q x b)
data Compose p q a b = forall x. Compose (p a x) (q x b)

instance (Profunctor p, Profunctor q) => Profunctor (Compose p q) where
  dimap con pro (Compose pax qxb) = Compose (dimap con id pax) (dimap id pro qxb)

data TenProd p q a b x y = TenProd (p a y) (q x b)

instance (Profunctor p, Profunctor q) => Profunctor (TenProd p q a b) where
  dimap con pro (TenProd pay qxb) = TenProd (dimap id pro pay) (dimap con id qxb)

type Compose' p q a b = Coend (TenProd p q a b)

-- The Yoneda Lemma
newtype PreYoneda f a x y = PreYoneda ((a -> x) -> f y)

instance Functor f => Profunctor (PreYoneda f a) where
  dimap con pro (PreYoneda ax_fy) = PreYoneda (\ax' -> fmap pro (ax_fy (con . ax')))

-- End (PreYoneda f a) ~ forall x. PreYoneda f a x x
newtype Yoneda f a = Yoneda (forall x. (a -> x) -> f x)

fromY :: Yoneda f a  -> f a
fromY (Yoneda axfx) = axfx id

toY :: Functor f => f a -> Yoneda f a
toY fa = Yoneda (`fmap` fa)

-- type CoYoneda f a = exists x. (x -> a, f x)
data CoYoneda f a = forall x. CoYoneda (x -> a) (f x)

fromCY :: Functor f => CoYoneda f a -> f a
fromCY (CoYoneda f fx) = fmap f fx

toCY :: f a -> CoYoneda f a
toCY = CoYoneda id
