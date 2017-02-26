module Palindrome where

import           Control.Monad (forever)
import           Data.Char     (toLower, isAlpha)
import           System.Exit   (exitSuccess)

isPalindrome :: String -> Bool
isPalindrome s = canonical == reverse canonical
  where
    canonical = map toLower filtered
    filtered = filter isAlpha s

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  if isPalindrome line1
    then putStrLn "It's a palindrome!"
    else do
      putStrLn "Nope!"
      exitSuccess
