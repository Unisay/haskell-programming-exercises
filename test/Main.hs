module Main where

import           Test.Hspec
import           Test.QuickCheck
import           RecursionSpec   (recursionSpec)
import           WordNumberSpec  (wordNumberSpec)
import           SemigroupSpec   (semigroupSpec)
import           MonoidSpec      (monoidSpec)
import           FunctorSpec     (functorSpec)
import           ApplicativeSpec (applicativeSpec)

half :: Fractional a => a -> a
half x = x / 2

-- for any list you apply sort to this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, _) = (Just y, x >= y)

main :: IO ()
main = hspec $ do
  wordNumberSpec
  recursionSpec
  semigroupSpec
  monoidSpec
  functorSpec
  applicativeSpec
  describe "half" $ do
    it "half is lower than whole" $
      property (\a -> abs (half a) <= abs (a :: Double))
    it "half * 2 == whole" $
      property (\a -> half a * 2 == (a :: Double))
--   describe "listOrdered" $
--     it "[1, 2, 3] is ordered" $
--       property (\x -> listOrdered (x :: Ord x => x))
