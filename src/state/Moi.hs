{-# LANGUAGE InstanceSigs #-}

module Moi where

import Control.Arrow (first)

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ first f . g

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \x -> (a, x)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> k s where
    k s0 = let (h, s1) = f s0
               (a, s2) = g s1
           in (h a, s2)

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (>>=) (Moi f) g = Moi $ \s -> k s where
    k s0 = let (a, s1) = f s0 in runMoi (g a) s1
