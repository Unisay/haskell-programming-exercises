module BasicDerivedOpsExercises where

{-
   Implement the functions in terms of foldMap or foldr from Foldable,
   then try them out with multiple types that have Foldable instances.
-}

import Prelude hiding (sum, foldr)
import Data.Monoid
import Data.Foldable

sum :: (Foldable t, Num a) => t a -> a
sum t = getSum $ foldMap Sum t

sum' :: (Foldable t, Num a) => t a -> a
sum' = undefined

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

