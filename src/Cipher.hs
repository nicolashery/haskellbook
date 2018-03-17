module Cipher where

import Control.Monad (forever)
import Data.Char (chr, ord)
import System.Exit (exitSuccess)

main :: IO ()
main = forever $ do
  let caesarShift = 5
  let vigenereKeyword = "penguin"
  putStrLn "1. Caesar (encode)"
  putStrLn "2. Caesar (decode)"
  putStrLn "3. Vigenère (encode)"
  putStrLn "4. Vigenère (decode)"
  putStrLn "0. Exit"
  putStr "Chose an option: "
  option <- getLine
  case option of
    "1" -> do
      message <- promptMessage
      putStrLn $ "Encoded message: " ++ caesar caesarShift message
    "2" -> do
      message <- promptMessage
      putStrLn $ "Decoded message: " ++ unCaesar caesarShift message
    "3" -> do
      message <- promptMessage
      putStrLn $ "Encoded message: " ++ vigenere vigenereKeyword message
    "4" -> do
      message <- promptMessage
      putStrLn $ "Decoded message: " ++ unVigenere vigenereKeyword message
    "0" -> exitSuccess
    _ -> putStrLn "Must pick a number"

promptMessage :: IO String
promptMessage = do
  putStr "Enter message: "
  getLine

caesar :: Int -> String -> String
caesar shiftBy = map (shiftLetter shiftBy)

unCaesar :: Int -> String -> String
unCaesar shiftBy = map (shiftLetter (-shiftBy))

vigenere :: String -> String -> String
vigenere keyword message = map shiftPair pairs
  where
    messageNoSpaces = filter (/=' ') message
    pairs = zip messageNoSpaces (concat $ repeat keyword)
    shiftPair (l, c) = shiftLetter (charToShift c) l

unVigenere :: String -> String -> String
unVigenere keyword encoded = map unshiftPair pairs
  where
    encodedNoSpaces = filter (/=' ') encoded
    pairs = zip encodedNoSpaces (concat $ repeat keyword)
    unshiftPair (l, c) = shiftLetter (negate $ charToShift c) l

shiftLetter :: Int -> Char -> Char
shiftLetter shiftBy letter
  | letterPos >= lowerAPos && letterPos <= lowerZPos =
      chr $ shiftWithLowerBound lowerAPos letterPos
  | letterPos >= upperAPos && letterPos <= upperZPos =
      chr $ shiftWithLowerBound upperAPos letterPos
  | otherwise =
      chr (letterPos + shiftBy)
  where letterPos = ord letter
        lowerAPos = ord 'a'
        lowerZPos = ord 'z'
        upperAPos = ord 'A'
        upperZPos = ord 'Z'
        shiftWithLowerBound lower pos =
          lower + ((pos - lower + shiftBy) `mod` 26)

charToShift :: Char -> Int
charToShift c
  | charPos >= lowerAPos = charPos - lowerAPos
  | charPos >= upperAPos = charPos - upperAPos
  | otherwise = charPos
  where charPos = ord c
        lowerAPos = ord 'a'
        upperAPos = ord 'A'
