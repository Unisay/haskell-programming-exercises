{-# LANGUAGE InstanceSigs #-}

module RollingDices where

import System.Random
import Prelude hiding (sum, log)

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = fst . rollsCountLogged n

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 0 [] where
  go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
  go sum cnt log gen
    | sum >= n = (cnt, log)
    | otherwise =
      let (die, nextGen) = randomR (1, 6) gen
      in go (sum + die) (cnt + 1) (intToDie die : log) nextGen
