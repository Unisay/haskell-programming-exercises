module IntParser where

import Text.Trifecta hiding (digit)
import Data.Char
import Data.Maybe (maybe)

parseDigit :: Parser Char
parseDigit = oneOf "0123456789" <?> "digit"

base10Integer :: Parser Integer
base10Integer = unDigits 10 <$> digits <?> "integer" where
  digits = some digit
  digit = charToDigit <$> parseDigit
  charToDigit c = toInteger $ ord c - ord '0'
  unDigits base = foldl (\a b -> a * base + b) 0

base10Integer' :: Parser Integer
base10Integer' = maybeNegate <$> parseSign <*> base10Integer where
  maybeNegate = maybe id (const negate)
  parseSign = optional $ char '-'


