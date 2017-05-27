module Commute where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe

foo :: ReaderT Char Maybe String
foo = reader show

bar :: MaybeT (Reader Char) String
bar = MaybeT $ reader (Just . show)

{-
  > let mfoo = fmap (++"!") foo
  > let mbar = fmap (++"!") bar
  > (runReaderT mfoo) 'C'
  Just "'C'!"
  > (runReader (runMaybeT mbar)) 'C'
  Just "'C'!"
-}
