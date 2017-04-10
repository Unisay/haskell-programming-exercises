module InUse where

import Control.Applicative
import Data.List (elemIndex)

-- 1
added :: Maybe Integer
added = fmap (+3) (lookup 3 $ zip [1 :: Integer, 2, 3] [4, 5, 6])

-- 2
y :: Maybe Integer
y = lookup 3 $ zip [1 :: Integer, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1 :: Integer, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = liftA2 (,) y z

tupled' :: Maybe (Integer, Integer)
tupled' = (,) <$> y <*> z

-- 3
x' :: Maybe Int
x' = elemIndex 3 [1 :: Int, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1 :: Int, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'

-- 4
xs :: [Integer]
xs = [1, 2, 3]

ys :: [Integer]
ys = [4, 5, 6]

mx :: Maybe Integer
mx = lookup 3 $ zip xs ys

my :: Maybe Integer
my = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> liftA2 (,) mx my -- sum threats (,) as Foldable thus only taking snd

-- Write an Applicative instance for Identity

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)


-- Write an Applicative instance for Constant

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant a) <*> (Constant b) = Constant (a `mappend` b)

-- Using the Maybe Applicative

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if length s > maxLen
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = Address <$> validateLength 100 a

data Person = Person Name Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a
