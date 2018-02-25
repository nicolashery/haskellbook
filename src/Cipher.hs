module Cipher where

import Data.Char (chr, ord)

caesar :: Int -> String -> String
caesar shiftBy = map (shiftLetter shiftBy)

unCaesar :: Int -> String -> String
unCaesar shiftBy = map (shiftLetter (-shiftBy))

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
