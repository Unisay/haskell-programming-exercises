module ReaderT where

import Data.Functor.Identity
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

type Reader r a = ReaderT r Identity a

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT r) = ReaderT $ (fmap . fmap) f r

instance (Applicative m) => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  (ReaderT f) <*> (ReaderT r) = ReaderT $ (<*>) <$> f <*> r

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (>>=) (ReaderT rma) f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

instance MonadTrans (ReaderT r) where
  lift = liftReaderT

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  liftIO = lift . liftIO

ask :: ReaderT a IO a
ask = ReaderT pure
