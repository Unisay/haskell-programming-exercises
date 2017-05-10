{-# LANGUAGE InstanceSigs #-}

module Moi where

import Control.Arrow (first)

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State $ first f . g

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \x -> (a, x)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (State f) <*> (State g) = State $ \s0 -> let (h, _) = f s0
                                               (a, _) = g s0
                                           in (h a, s0)

instance Monad (State s) where
  return = pure

  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) (State f) g = State $ \s -> let (a, s') = f s
                                    in runState (g a) s'
