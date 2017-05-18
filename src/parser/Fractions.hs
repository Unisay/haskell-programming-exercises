{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Text.Trifecta
import Control.Applicative
import Data.Ratio ((%))
import Data.String (IsString)
import Data.Attoparsec.Text (parseOnly)

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m) => m Rational
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
  -- parseOnly is Attoparsec
  print $ parseOnly parseFraction badFraction
  print $ parseOnly parseFraction shouldWork
  print $ parseOnly parseFraction shouldAlsoWork
  print $ parseOnly parseFraction alsoBad

  -- parseString is Trifecta
  print $ parseString parseFraction mempty "1/2"
  print $ parseString parseFraction mempty "2/1"
  print $ parseString parseDecOrFrac mempty "10"
  print $ parseString parseFraction mempty "1/0"













