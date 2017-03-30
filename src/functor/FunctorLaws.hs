module FunctorLaws where

{-# ANN module "HLint: ignore Functor law" #-}

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose p q x = fmap q (fmap p x) == fmap (q . p) x
