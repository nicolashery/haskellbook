module Hangman where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "/usr/share/dict/words"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in l >= minWordLength
             && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle
  { word :: String
  , discovered :: [Maybe Char]
  , guessed :: [Char]
  }

instance Show Puzzle where
  show p =
    intersperse ' ' (fmap renderPuzzleChar (discovered p))
    ++ " Guessed so far: " ++ guessed p

freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle
  { word = w
  , discovered = map (const Nothing) w
  , guessed = []
  }

charInWord :: Puzzle -> Char -> Bool
charInWord p c = c `elem` word p

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed p c = c `elem` guessed p

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter p c =
  Puzzle (word p) newDiscovered (c : guessed p)
  where zipper guessChar wordChar discoveredChar =
          if wordChar == guessChar
          then Just wordChar
          else discoveredChar
        newDiscovered =
          zipWith (zipper c) (word p) (discovered p)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling it in accordingly."
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver p =
  if length (guessed p) > 7 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ (word p)
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin p =
  if all isJust (discovered p) then
    do putStrLn "You win!"
       exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  w <- randomWord'
  let puzzle = freshPuzzle (fmap toLower w)
  runGame puzzle
