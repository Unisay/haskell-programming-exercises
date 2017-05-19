{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SemVerParser where

import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Text.Trifecta hiding (release)

data NumberOrString = NOSS String | NOSI Integer
  deriving Show

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
  deriving Show

instance Eq SemVer where
  (==) (SemVer mj mn pt _ _) (SemVer mj' mn' pt' _ _)
    | mj' == mj && mn' == mn && pt' == pt = True
    | otherwise = False

instance Ord SemVer where
  compare (SemVer mj mn pt _ _) (SemVer mj' mn' pt' _ _) =
    compare [mj, mn, pt] [mj', mn', pt']

parseMajor :: Parser Major
parseMajor = integer

parseMinor :: Parser Minor
parseMinor = char '.' *> integer

parsePatch :: Parser Patch
parsePatch = char '.' *> integer

alphaStr :: Parser String
alphaStr = some (noneOf ".+")

parseNOS :: Parser NumberOrString
parseNOS = fmap NOSI integer <|> fmap NOSS alphaStr

parseSegments :: Parser [NumberOrString]
parseSegments = parseNOS `sepBy` (NOSS <$> symbol ".")

parseRelease :: Parser Release
parseRelease = char '-' *> parseSegments

parseMetadata :: Parser Metadata
parseMetadata = char '+' *> parseSegments

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- parseMajor
  minor <- parseMinor
  patch <- parsePatch
  release <- optional parseRelease
  metadata <- optional parseMetadata
  _ <- eof
  return $ SemVer major minor patch (fromMaybe [] release) (fromMaybe [] metadata)


