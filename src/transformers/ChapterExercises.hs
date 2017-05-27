module ChapterExercises where

import ReaderT
import StateT
import Data.Functor.Identity
import Control.Monad.IO.Class

-- rDec is a function that should get its argument
-- in the context of Reader and return a value decremented by one.
rDec :: Num a => Reader a a
rDec = ReaderT $ return . subtract 1 -- subtract 1 <$> ask

-- rShow is show, but in Reader.
rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ return . show -- show <$> ask

-- first print the input with a greeting,
-- then return the input incremented by one.
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
  n <- ask
  liftIO $ putStr "Hi: " *> print n
  return (n + 1)

-- prints the input with a greeting,
-- then puts the incremented input as the new state,
-- and returns the original input as a String.
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = do
  i <- StateT $ \s -> pure (show s, s + 1)
  liftIO $ putStr "Hi: " *> putStrLn i
  return i
