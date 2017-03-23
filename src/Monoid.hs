module Monoid where

data Optional a = Nada | Only a
                  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where

  mempty = Nada

  mappend Nada _ = Nada
  mappend _ Nada = Nada
  mappend (Only l) (Only r) = Only $ l `mappend` r

