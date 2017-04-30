{-# LANGUAGE InstanceSigs #-}

module ReaderInstances where

import Control.Applicative (liftA2)

newtype HumanName = HumanName String  deriving (Eq, Show)
newtype DogName   = DogName String    deriving (Eq, Show)
newtype Address   = Address String    deriving (Eq, Show)

data Person =
  Person { humanName :: HumanName
         , dogName   :: DogName
         , address   :: Address
         } deriving (Eq, Show)

data Dog =
  Dog { dogsName    :: DogName
      , dogsAddress :: Address
      } deriving (Eq, Show)

pers :: Person
pers = Person
  (HumanName "Big Bird")
  (DogName "Barkley")
  (Address "Sesame Street")

chris :: Person
chris = Person
  (HumanName "Chris Allen")
  (DogName "Papu")
  (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- with Reader
getDogR :: Reader Person Dog
getDogR = Reader $ Dog <$> dogName <*> address

-- with Reader, alternate
getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

-- with Reader Monad
getDogRM :: Reader Person Dog
getDogRM = do
  name <- asks dogName
  addy <- asks address
  return $ Dog name addy

newtype Reader r a = Reader { runReader :: r -> a }

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 fab fa fb = fmap fab fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= arb = Reader (\r -> let (Reader rb) = arb (ra r) in rb r)
