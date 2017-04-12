module ApplicativeSpec (applicativeSpec) where

import           ApplicativeLaws
import           Test.Hspec
import           Test.QuickCheck
import           Control.Applicative (pure, (<*>))

instance Show (a -> b) where
  show f = "<function>"

applicativeSpec :: SpecWith ()
applicativeSpec =
  describe "Applicative laws" $ do
    it "identity law" $
      property (identityLaw :: String -> Bool)
    it "composition law" $
      property (compositionLaw :: [Int -> Char] -> [Bool -> Int] -> [Bool] -> Bool)
    it "homomorphism law" $
      property (homomorphismLaw (pure :: a -> [a]) (<*>) :: (Int -> Char) -> Int -> Bool)
