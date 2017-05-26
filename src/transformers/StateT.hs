module StateT where

import Control.Monad.Trans.Class

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT g) = StateT $ \s -> c <$> g s where c (a, s) = (f a, s)

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  (<*>) (StateT smf) (StateT smr) = StateT $ \s0 -> do
    (f, s1) <- smf s0
    (r, s2) <- smr s1
    return (f r, s2)

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (>>=) (StateT sma) f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> (\a -> (a, s)) <$> ma
