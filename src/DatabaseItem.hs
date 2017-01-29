{-# LANGUAGE LambdaCase #-}

module DatabaseItem where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

-- Write a function that filters for DbDate values and returns a list of the UTCTime values inside them.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
  where f (DbDate t) b = t : b
        f _ b = b

filterDbDate2 :: [DatabaseItem] -> [UTCTime]
filterDbDate2 [] = []
filterDbDate2 (DbDate x : xs) = x : filterDbDate2 xs
filterDbDate2 (_ : xs) = filterDbDate2 xs

-- filterDbDate xs = [ x | DbDate x <- xs ]
-- filterDbDate db = map (\case DbDate t -> t) (filter (\case DbDate _ -> True; _ -> False) db)

-- Write a function that filters for DbNumber values and returns a list of the Integer values inside them.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f [] where
  f (DbNumber n) b = n : b
  f _ b = b

-- Write a function that gets the most recent date.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr max (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)) . filterDbDate

-- Write a function that sums all of the DbNumber values.
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- Write a function that gets the average of the DbNumber values.
-- You'll probably need to use fromIntegral to get from Integer to Double.
avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sumDb db) / fromIntegral (length db)
