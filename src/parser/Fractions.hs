module Text.Fractions where

import Text.Trifecta
import Control.Applicative
import Data.Ratio ((%))

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseDecOrFrac :: Parser (Either Integer Rational)
parseDecOrFrac = dec <|> frac where
  dec = Left <$> decimal
  frac = Right <$> parseFraction

main :: IO ()
main = do
  print $ parseString parseFraction mempty "1/2"
  print $ parseString parseFraction mempty "2/1"
  print $ parseString parseDecOrFrac mempty "10"
  print $ parseString parseFraction mempty "1/0"













