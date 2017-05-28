{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Morra where

{-
  Morra is a hand game that dates back thousands of years to ancient
  Roman and Greek times. Each player simultaneously reveals their hand,
  extending any number of fingers, and calls out a number.
  Any player who successfully guesses the total number of fingers
  revealed by all players combined scores a point.
-}

import Prelude hiding (sum, log, round)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Applicative
import System.IO (hFlush, stdout)
import System.Random
import Text.Read (readMaybe)
import Data.Maybe (maybeToList)

newtype Fingers = Fingers Int deriving (Eq, Num)

instance Show Fingers where
  show (Fingers 1) = "one finger"
  show (Fingers 2) = "two fingers"
  show (Fingers 3) = "three fingers"
  show (Fingers 4) = "four fingers"
  show (Fingers 5) = "five fingers"
  show _ = error "Invalid fingers"

(<->) :: Ord a => a -> a -> a -> Bool
(<->) l u = (&&) <$> (>= l) <*> (<= u)

mkFingers :: String -> Maybe Fingers
mkFingers s = Fingers <$> mfilter (1 <-> 5) (readMaybe s)

newtype Guess = Guess Int deriving (Eq, Num)

mkGuess :: String -> Maybe Guess
mkGuess s = Guess <$> mfilter (2 <-> 10) (readMaybe s)

instance Show Guess where show (Guess n) = show n


data Turn = MkTurn { fingers :: Fingers, guess :: Guess }

instance Show Turn where
 show (MkTurn f g) = show f ++ " while guessing sum " ++ show g

data Winner = User | Robot | Nobody deriving (Eq, Show)

type QuitOr a = MaybeT IO a

mayQuit :: Maybe a -> QuitOr a
mayQuit = MaybeT . pure

ask :: String -> QuitOr String
ask s = do
  lift $ putStr (s ++ " > ")
  lift $ hFlush stdout
  answer <- lift getLine
  when (answer == "q") mzero
  return answer

askUntil :: (String -> Maybe a) -> String -> QuitOr a
askUntil check question = do
  answer <- ask question
  mayQuit (check answer) <|> askUntil check question

-- Asks user to make a turn until he provides a correct one
-- (Just) or quits (Nothing)
userTurns :: QuitOr Turn
userTurns = do
  f <- askUntil mkFingers
    "Please enter a number of fingers to show (1, 2, 3, 4, 5)"
  g <- askUntil mkGuess
    "Try to guess the sum (2, 3, 4, 5, 6, 7, 8, 9, 10)"
  return $ MkTurn f g

randomFingers :: IO Fingers
randomFingers = Fingers <$> getStdRandom (randomR (1, 5))

randomGuess :: IO Guess
randomGuess = Guess <$> getStdRandom (randomR (2, 10))

robotTurns :: IO Turn
robotTurns = MkTurn <$> randomFingers <*> randomGuess

determineWinner :: Turn -> Turn -> (Int, Winner)
determineWinner usersTurn robotsTurn =
  let (Fingers numFingersUser) = fingers usersTurn
      (Fingers numFingersRobot) = fingers robotsTurn
      (Guess usersGuess) = guess usersTurn
      (Guess robotsGuess) = guess robotsTurn
      sum = numFingersUser + numFingersRobot
      d1 = abs (sum - usersGuess)
      d2 = abs (sum - robotsGuess)
      winner = case compare d1 d2 of
        LT -> User
        GT -> Robot
        EQ -> Nobody
  in (sum, winner)

round :: QuitOr Winner
round = do
  usersTurn <- userTurns
  say $ "You showed " ++ show usersTurn
  robotsTurn <- lift robotTurns
  say $ "Robot showed " ++ show robotsTurn
  let (sm, winner) = determineWinner usersTurn robotsTurn
  say $ "Sum is " ++ show sm ++ " so " ++ show winner ++ " wins this round!"
  return winner
  where say = liftIO . putStrLn

nextRound :: MaybeT (StateT [Winner] IO) ()
nextRound = mapMaybeT makeLog round
  where
    makeLog io = StateT $ \log -> saveWinner log <$> io
    saveWinner log win = (void win, log ++ maybeToList win)

playUntilQuit :: IO [Winner]
playUntilQuit = runLog loop
  where
    loop = runMaybeT $ forever nextRound
    runLog g = execStateT g []

summary :: [Winner] -> (Int, Int, Winner)
summary log =
  let countVictories p = length . filter (== p)
      userVictories = countVictories User log
      robotVictories = countVictories Robot log
      winner = case compare userVictories robotVictories of
        GT -> User
        LT -> Robot
        EQ -> Nobody
  in (userVictories, robotVictories, winner)

main :: IO ()
main = do
  putStrLn "This is Morra."
  putStrLn "Enter 'q' at any time in order to quit."
  putStrLn "Press Enter when ready to play."
  _ <- getLine
  (userVictories, robotVictories, overallWinner) <- summary <$> playUntilQuit
  putStrLn $ "User has won " ++ show userVictories ++ " times"
  putStrLn $ "Robot has won " ++ show robotVictories ++ " times"
  putStrLn $ if overallWinner == Nobody
             then "Congratulations to both players!"
             else "Congratulations to the " ++ show overallWinner ++ "!"



