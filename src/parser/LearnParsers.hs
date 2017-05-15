module LearnParsers where

import Text.Trifecta (Parser, unexpected, char, parseString, eof, string, choice)

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = do
  c <- char '1'
  _ <- eof
  return c

one' :: Parser Char
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2' <* eof

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

oneTwoThree :: Parser String
oneTwoThree = choice [string "123", string "12", string "1"]

string' :: String -> Parser String
string' = traverse char

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)

main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
