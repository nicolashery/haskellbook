module Morra where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT(..), get, modify, runStateT)
import System.Random (randomRIO)

data Player = Odds | Evens
  deriving (Eq, Show)

data Hand = One | Two
  deriving (Eq, Show)

data Turn = Turn
  { turnOdds :: Hand
  , turnEvens :: Hand
  } deriving (Eq, Show)

data Score = Score
  { scoreOdds :: Int
  , scoreEvens :: Int
  } deriving (Eq, Show)

data GameState = GameState Score
  deriving (Eq, Show)

turnWinner :: Turn -> Player
turnWinner t =
  if even sumHands
  then Evens
  else Odds
  where
    sumHands :: Int
    sumHands =
      (handToInt $ turnOdds t) + (handToInt $ turnEvens t)

handToInt :: Hand -> Int
handToInt h =
  case h of
    One -> 1
    Two -> 2

bumpScore :: Score -> Player -> Score
bumpScore s p =
  case p of
    Odds -> s { scoreOdds = scoreOdds s + 1 }
    Evens -> s { scoreEvens = scoreEvens s + 1 }

initialScore :: Score
initialScore = Score 0 0

initialGameState :: GameState
initialGameState = GameState initialScore

showTurn :: Turn -> String
showTurn t =
  mconcat [ "Odds played "
          , show $ handToInt (turnOdds t)
          , ", Evens played "
          , show $ handToInt (turnEvens t)
          ]

showWinner :: GameState -> String
showWinner (GameState score) =
  case compare (scoreOdds score) (scoreEvens score) of
    EQ -> "It's a tie!"
    GT -> "Congratulations, player Odds wins!"
    LT -> "Congratulations, player Evens wins!"

showScore :: GameState -> String
showScore (GameState score) =
  mconcat [ "Odds "
          , show $ scoreOdds score
          , ", "
          , "Evens "
          , show $ scoreEvens score
          ]

randomHandIO :: IO Hand
randomHandIO = intToHand <$> randomRIO (1, 2)
  where
    intToHand :: Int -> Hand
    intToHand n =
      case n of
        1 -> One
        2 -> Two
        _ -> error $ "intToHand got non 1-2 integer: " ++ show n

handleTurn :: Hand -> StateT GameState IO ()
handleTurn playerHand = do
  computerHand <- liftIO randomHandIO
  let turn = Turn { turnOdds = playerHand, turnEvens = computerHand }
  liftIO $ putStrLn (showTurn turn)
  modify $ bumpScore' turn
  return ()
  where
    bumpScore' :: Turn -> GameState -> GameState
    bumpScore' t (GameState score) =
      GameState $ bumpScore score (turnWinner t)

gameLoop :: StateT GameState IO ()
gameLoop = do
  s <- get
  liftIO $ putStrLn $ "Current score is: " ++ showScore s
  liftIO $ putStrLn "Play a hand! Enter '1' or '2' (or 'q' to end game):"
  hand <- liftIO getLine
  case hand of
    "1" -> handleTurn One >> gameLoop
    "2" -> handleTurn Two >> gameLoop
    "q" -> return ()
    _ -> do
      liftIO $ putStrLn "Sorry, I didn't understand what you meant"
      gameLoop

main :: IO ()
main = do
  putStrLn "Welcome to Morra (Odds & Evens)!"
  putStrLn "You are player Odds"
  (_, endingState) <- runStateT gameLoop initialGameState
  putStrLn $ showWinner endingState
  putStrLn "Game ended"

-- TODO: human vs human mode, improve computer AI
