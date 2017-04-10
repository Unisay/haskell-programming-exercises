module ApplicativeSpec (applicativeSpec) where

import           ApplicativeLaws
import           Test.Hspec
import           Test.QuickCheck
import           Control.Applicative (pure, (<*>))

type S = String
type B = Bool

instance Show (a -> b) where
  show f = "<function>"

applicativeSpec :: SpecWith ()
applicativeSpec =
  describe "Applicative laws" $ do
    it "identity law" $
      property (identityLaw :: [S] -> Bool)
    it "composition law" $
      property (compositionLaw :: [Int -> String] -> [Bool -> Int] -> [Bool] -> Bool)
    it "homomorphism law" $
      property (homomorphismLaw (pure :: a -> [a]) (<*>) :: (Int -> String) -> Int -> Bool)
