module Text.Fractions where

import Text.Trifecta
import Data.Ratio ((%))

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

main :: IO ()
main = do
  print $ parseString parseFraction mempty "1/2"
  print $ parseString parseFraction mempty "2/1"
  print $ parseString parseFraction mempty "10"
  print $ parseString parseFraction mempty "1/0"













