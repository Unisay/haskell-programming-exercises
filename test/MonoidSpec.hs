module MonoidSpec (monoidSpec) where

import           Data.Monoid
import           Monoid
import           Test.Hspec

int :: Int
int = 42

monoidSpec :: SpecWith ()
monoidSpec = describe "Monoid (Optional a)" $ do
 it "mappend Only (Sum int)) (Only (Sum int) == Only" $
   mappend (Only (Sum int)) (Only (Sum int)) `shouldBe` Only 84
 it "mappend Only (Product int)) (Only (Product int) == Only" $
   mappend (Only (Product int)) (Only (Product int)) `shouldBe` Only 1764
 it "mappend Only Only == Only" $
   mappend (Only (Sum int)) (Only (Sum int)) `shouldBe` Only 84
 it "mappend Nada Only == Nada" $
   mappend Nada (Only (Sum int)) `shouldBe` Nada
 it "mappend Only Nada == Nada" $
   mappend (Only [int]) Nada `shouldBe` Nada
