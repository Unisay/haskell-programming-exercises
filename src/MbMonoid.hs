module MbMonoid where

data Mb a = It a | None
         deriving (Eq, Show)

instance Monoid (Mb a) where
  mempty = None
  mappend None _ = None
  mappend _ None = None
  mappend (It l) (It _) = It l

