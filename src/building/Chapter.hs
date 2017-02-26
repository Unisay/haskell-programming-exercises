module Chapter where

import           Data.Char               (isDigit)
import           System.Console.Readline (readline)
import           System.Exit             (exitSuccess)
import           System.IO               (hFlush, stdout)

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

readField :: String -> (String -> Bool) -> (String -> t) -> IO t
readField field test transf = do
  putStrLn $ "Please enter \x1b[32m" ++ field ++ "\x1b[0m or type '/exit' to quit program"
  hFlush stdout
  maybeLine <- readline "> "
  case maybeLine of
    (Just "/exit") -> exitSuccess
    (Just line)    | test line -> return $ transf line
    _              -> readField field test transf

gimmePerson :: IO ()
gimmePerson = do
  name <- readField "your name" (not . null) id
  age <- readField "your age" (all isDigit) (read :: String -> Integer)
  case mkPerson name age of
    (Left  NameEmpty) -> putStrLn "Name is empty"
    (Left  AgeTooLow) -> putStrLn "Age is too low"
    (Left  (PersonInvalidUnknown msg)) -> putStrLn $ "Error occurred: " ++ msg
    (Right (Person name age)) -> putStrLn $ "Yay! Successfully got " ++ name ++ " who is " ++ show age
