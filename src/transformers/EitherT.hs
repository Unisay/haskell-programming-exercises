{-# LANGUAGE ScopedTypeVariables #-}

module EitherT where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT f) <*> (EitherT a) = EitherT $ (<*>) <$> f <*> a

instance Monad m => Monad (EitherT e m) where
  return = pure
  (>>=) :: forall a b . EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT m) >>= f = EitherT $ m >>= g where
    g :: Either e a -> m (Either e b)
    g (Left e) = return $ Left e
    g (Right a) = runEitherT (f a)
