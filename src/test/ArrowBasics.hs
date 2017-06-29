{-# LANGUAGE Arrows, TypeOperators #-}

module ArrowBasics where

import Control.Arrow( Arrow, ArrowChoice, ArrowLoop, Kleisli(..)
                    , arr, first, second, (***), (&&&)
                    , left, right, (+++), (|||)
                    , loop )
import Control.Category (Category, (>>>), (.), id)
import Control.Monad (liftM)
import Data.Function (fix)
import Prelude hiding((.), id)

-- Count the occurrences of word w in a string
count :: String -> String -> Int
count w = length . filter (== w) . words

-- Count the occurrences of word w in a file and print the count.
countFile :: String -> FilePath -> IO ()
countFile w = (>>= print) . liftM (count w) . readFile

countFileA :: String -> Kleisli IO FilePath ()
countFileA w = Kleisli readFile >>> arr (count w) >>> Kleisli print

listcase :: [t] -> Either () (t, [t])
listcase []     = Left  ()
listcase (x:xs) = Right (x,xs)

mapA :: ArrowChoice a => a b c -> a [b] [c]
mapA f = fromList >>> (base ||| recur f)
  where
    fromList :: Arrow a => a [b] (Either () (b, [b]))
    fromList = arr listcase

    base :: Arrow a => a () [c]
    base = arr (const [])

    recur :: ArrowChoice a => a b c -> a (b, [b]) [c]
    recur h = h *** mapA h >>> toList

    toList :: Arrow a => a (x, [x]) [x]
    toList = arr $ uncurry (:)
