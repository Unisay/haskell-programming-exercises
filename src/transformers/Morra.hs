{-# LANGUAGE QuasiQuotes, GeneralizedNewtypeDeriving #-}

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
import Text.RawString.QQ

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

data Player = Human1 | Human2 | Robot deriving Eq

instance Show Player where
  show Human1 = "Player #1"
  show Human2 = "Player #2"
  show Robot  = "Robot"

data Mode = HumanVsRobot | HumanVsHuman deriving (Eq, Show)

newtype Winner = Winner (Maybe Player) deriving Eq

data RoundResult = RoundResult { winner :: Winner, totalFingers :: Int }

players :: Mode -> (Player, Player)
players HumanVsRobot = (Human1, Robot)
players HumanVsHuman = (Human1, Human2)

mkMode :: String -> Maybe Mode
mkMode "1" = Just HumanVsRobot
mkMode "2" = Just HumanVsHuman
mkMode _ = Nothing

data Turn = Turn Player Fingers Guess

instance Show Turn where
 show (Turn p f g) =
   mconcat [show p, " shows ", show f, " while guessing sum ", show g]

type QuitOr a = MaybeT IO a

ask :: String -> QuitOr String
ask s = mfilter (/="q") . lift $ answer
 where answer = putStr (s ++ " > ") >> hFlush stdout >> getLine

mayQuit :: Maybe a -> QuitOr a
mayQuit = MaybeT . pure

askUntil :: (String -> Maybe a) -> String -> QuitOr a
askUntil check question = do
  answer <- ask question
  mayQuit (check answer) <|> askUntil check question

-- Asks user to make a turn until he provides a correct one
-- (Just) or quits (Nothing)
humanTurns :: Mode -> Player -> QuitOr Turn
humanTurns mode player = do
  f <- askUntil mkFingers $
    playerName mode player ++ "enter a number of fingers to show (1, 2, 3, 4, 5)"
  g <- askUntil mkGuess $
    playerName mode player ++ "try to guess the sum (2, 3, 4, 5, 6, 7, 8, 9, 10)"
  return $ Turn player f g
    where
      playerName :: Mode -> Player -> String
      playerName HumanVsRobot _ = "Please "
      playerName HumanVsHuman p = show p ++ ", please "

randomFingers :: IO Fingers
randomFingers = Fingers <$> getStdRandom (randomR (1, 5))

randomGuess :: IO Guess
randomGuess = Guess <$> getStdRandom (randomR (2, 10))

robotTurns :: IO Turn
robotTurns = Turn Robot <$> randomFingers <*> randomGuess

determineWinner :: Turn -> Turn -> RoundResult
determineWinner turn1 turn2 =
  let (Turn player1 (Fingers fingers1) (Guess guess1)) = turn1
      (Turn player2 (Fingers fingers2) (Guess guess2)) = turn2
      fingers = fingers1 + fingers2
      d1 = abs (fingers - guess1)
      d2 = abs (fingers - guess2)
      theWinner = case compare d1 d2 of
        LT -> Winner $ Just player1
        GT -> Winner $ Just player2
        EQ -> Winner Nothing
  in RoundResult theWinner fingers

say :: String -> QuitOr ()
say = liftIO . putStrLn

turns :: Mode -> (QuitOr Turn, QuitOr Turn)
turns HumanVsRobot = (humanTurns HumanVsRobot Human1, lift robotTurns)
turns HumanVsHuman = (humanTurns HumanVsHuman Human1, humanTurns HumanVsHuman Human2)

round :: Mode -> QuitOr RoundResult
round mode = do
  let (quitOrFirstTurn, quitOrSecondTurn) = turns mode
  firstTurn <- quitOrFirstTurn
  say $ show firstTurn
  secondTurn <- quitOrSecondTurn
  say $ show secondTurn
  let result = determineWinner firstTurn secondTurn
  say $ mconcat [ "Sum is "
                , show (totalFingers result)
                , " so "
                , showWinner mode (winner result)
                , "!\n"
                ]
  return result
  where showWinner :: Mode -> Winner -> String
        showWinner _ (Winner Nothing) = "nobody wins, its a tie"
        showWinner _ (Winner (Just Robot)) = "robot wins"
        showWinner HumanVsRobot (Winner (Just _)) = "you win"
        showWinner HumanVsHuman (Winner (Just p)) = show p ++ " wins"

nextRound :: Mode -> MaybeT (StateT [RoundResult] IO) ()
nextRound mode = mapMaybeT makeLog (round mode)
  where
    makeLog io = StateT $ \log -> saveWinner log <$> io
    saveWinner log win = (void win, log ++ maybeToList win)

roundsUntilQuit :: Mode -> IO [RoundResult]
roundsUntilQuit mode = execStateT log []
  where log = runMaybeT $ forever (nextRound mode)

askMode :: QuitOr Mode
askMode = askUntil mkMode [r|Please make a choice:
 1  Human vs. Robot
 2  Human vs. Human
|]

summary :: (Player, Player) -> [RoundResult] -> (Int, Int, Winner)
summary (player1, player2) log = (player1Victories, player2Victories, theWinner)
  where
    player1Victories = countVictories player1 log
    player2Victories = countVictories player2 log
    countVictories p = length . filter ((== Winner (Just p)) . winner)
    theWinner = case compare player1Victories player2Victories of
      GT -> Winner $ Just player1
      LT -> Winner $ Just player2
      EQ -> Winner Nothing

congratulations :: Mode -> Winner -> String
congratulations _ (Winner Nothing) =
  "Congratulations to both players!"
congratulations _ (Winner (Just Robot)) =
  "Condolences, robot won :("
congratulations HumanVsRobot (Winner (Just _)) =
  "Congratulations, you won!"
congratulations HumanVsHuman (Winner (Just Human1)) =
  "Congratulations to the first player who has won the game!"
congratulations HumanVsHuman (Winner (Just Human2)) =
  "Congratulations to the second player who has won the game!"

main :: IO ()
main = void $ runMaybeT $ do
  say [r|
   • ▌ ▄ ·.       ▄▄▄  ▄▄▄   ▄▄▄·
   ·██ ▐███▪▪     ▀▄ █·▀▄ █·▐█ ▀█
   ▐█ ▌▐▌▐█· ▄█▀▄ ▐▀▀▄ ▐▀▀▄ ▄█▀▀█
   ██ ██▌▐█▌▐█▌.▐▌▐█•█▌▐█•█▌▐█ ▪▐▌
   ▀▀  █▪▀▀▀ ▀█▄▀▪.▀  ▀.▀  ▀ ▀  ▀
  |]
  say "This is a game Morra."
  say "Enter 'q' any time in order to finish the game.\n"
  mode <- askMode
  let ps @ (player1, player2) = players mode
  (p1wins, p2wins, overallWinner) <- lift $ summary ps <$> roundsUntilQuit mode
  say $ name mode player1 ++ " won " ++ show p1wins ++ " times"
  say $ name mode player2 ++ " won " ++ show p2wins ++ " times"
  say $ congratulations mode overallWinner
    where name HumanVsRobot Robot = "Robot has"
          name HumanVsRobot _ = "You"
          name HumanVsHuman p = show p ++ " has"



