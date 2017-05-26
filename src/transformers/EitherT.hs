{-# LANGUAGE ScopedTypeVariables #-}

module EitherT where

import Data.Either (either)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT f) <*> (EitherT a) = EitherT $ (<*>) <$> f <*> a

instance Monad m => Monad (EitherT e m) where
  return = pure
  (>>=) :: forall a b . EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT m) >>= f = EitherT $ m >>= g
    where
      g :: Either e a -> m (Either e b)
      g (Left e) = return $ Left e
      g (Right a) = runEitherT (f a)

instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift = EitherT . fmap Right

instance MonadIO m => MonadIO (EitherT e m) where
 liftIO :: IO a -> EitherT e m a
 liftIO = lift . liftIO

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT et = EitherT $ swapEither <$> runEitherT et
  where
    swapEither (Left l) = Right l
    swapEither (Right r) = Left r

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g et = runEitherT et >>= either f g
