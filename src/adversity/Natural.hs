module Natural where

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ pred) = 1 + natToInteger pred

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | i == 0 = Just Zero
  | otherwise = Just (Succ x) where Just x = integerToNat (i - 1)

