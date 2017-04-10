{-# LANGUAGE RankNTypes #-}

module ApplicativeLaws where

import Control.Applicative ()

identityLaw :: (Applicative v, Eq (v a)) => v a -> Bool
identityLaw v = (pure id <*> v) == v

compositionLaw :: (Applicative f, Eq (f c)) => f (b -> c) -> f (a -> b) -> f a -> Bool
compositionLaw fbc fab fa = (pure (.) <*> fbc <*> fab <*> fa) == (fbc <*> (fab <*> fa))

homomorphismLaw :: Eq (g b) => (forall c. c -> g c) ->
                               (forall c d. g (c -> d) -> g c -> g d) ->
                               (a -> b) -> a -> Bool
homomorphismLaw pure' ap f x = ap (pure' f) (pure' x) == pure' (f x)

interchangeLaw :: (Applicative f, Eq (f b)) => f (a -> b) -> a -> Bool
interchangeLaw fx y = (fx <*> pure y) == (pure ($ y) <*> fx)
