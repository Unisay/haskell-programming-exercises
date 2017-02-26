module Vigenere where

import           Control.Monad (forever, when)
import           Data.Char
import           Data.List

newtype Err = MkError String
data Mode = Encode | Decode

enumerate :: String -> [Int]
enumerate = cycle . map ord

shiftBounded :: Int -> Int -> Int -> Char -> Char
shiftBounded lower upper n c
  | pos < lower = chr $ dis + pos + 1
  | pos > upper = chr $ pos - dis - 1
  | otherwise = chr pos
  where
    pos = mod n dis + ord c
    dis = upper - lower

shiftR :: Char -> Int -> Char
shiftR c n
  | isUpper c = shiftBounded (ord 'A') (ord 'Z') n c
  | otherwise = shiftBounded (ord 'a') (ord 'z') n c

shiftL :: Char -> Int -> Char
shiftL c i = shiftR c (1 - i)

shiftWith :: (Char -> Int -> Char) -> String -> String -> String
shiftWith f pass s = zipWith f s (enumerate pass)

encode :: String -> String -> String
encode = shiftWith shiftR

decode :: String -> String -> String
decode = shiftWith shiftL

welcome :: IO ()
welcome = putStrLn "Welcome to the Vigenere cipher Encoder/Decoder!"

readLine :: String -> IO String
readLine msg = do
  putStrLn msg
  getLine

getMode :: IO (Either Err Mode)
getMode = do
  input <- readLine "In order to encode message press E, for decoding press D"
  return $ case input of
    ['D'] -> Right Decode
    ['E'] -> Right Encode
    _     -> Left $ MkError "Neither E nor D was entered!"

main :: IO ()
main = do
  welcome
  forever $ do
  errorOrMode <- getMode
  case errorOrMode of
    (Right Encode) -> handle encode "encode"
    (Right Decode) -> handle decode "decode"
    (Left (MkError errorMsg)) -> putStrLn errorMsg
    where
      handle f action = do
        word <- readLine $ "Please enter a word to " ++ action ++ " with the Vigenere cipher:"
        pass <- readLine "Please enter password:"
        putStrLn $ "Result: " ++ f pass word
