module MonoidSpec (monoidSpec) where

import           Data.Monoid
import           Monoid
import           Test.Hspec
import           Test.QuickCheck

type S = String
type B = Bool

int :: Int
int = 42

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

monoidSpec :: SpecWith ()
monoidSpec = do

  describe "Monoid laws" $ do
    it "is associative" $
      property (monoidAssoc :: S -> S -> S -> B)
    it "left identity" $
      property (monoidLeftIdentity :: S -> B)
    it "right identity" $
      property (monoidRightIdentity :: S -> B)

  describe "Monoid (Optional a)" $ do
    it "mappend Only (Sum int)) (Only (Sum int) == Only" $
      mappend (Only (Sum int)) (Only (Sum int)) `shouldBe` Only (Sum (int + int))
    it "mappend Only (Product int)) (Only (Product int) == Only" $
      mappend (Only (Product int)) (Only (Product int)) `shouldBe` Only (Product (int * int))
    it "mappend Nada Only == Nada" $
      mappend Nada (Only (Sum int)) `shouldBe` Nada
    it "mappend Only Nada == Nada" $
      mappend (Only [int]) Nada `shouldBe` Nada
