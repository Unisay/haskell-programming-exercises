module MaybeT where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  (MaybeT f) <*> (MaybeT a) = MaybeT $ (<*>) <$> f <*> a

instance Monad m => Monad (MaybeT m) where
  return = pure
  (MaybeT m) >>= f = MaybeT $ m >>= g where
    g (Just a) = runMaybeT (f a)
    g Nothing = return Nothing

instance MonadTrans MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift = MaybeT . fmap Just

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  liftIO = lift . liftIO
