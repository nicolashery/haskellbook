module Chapter08 where

import Data.List (intersperse)

-- Reviewing currying

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- Recursion

sumUpTo :: (Eq a, Num a) => a -> a
sumUpTo 0 = 0
sumUpTo n = n + sumUpTo (n - 1)

recMult :: (Integral a) => a -> a -> a
recMult x y
  | y > 0 = go y
  | y < 0 = negate $ go (abs y)
  | otherwise = 0
  where go 0 = 0
        go n = x + go (n - 1)

-- Fixing dividedBy

data DividedResult =
  Result Integer
  | DividedByZero
  deriving Show

dividedBy :: (Integral a) => a -> a -> DividedResult
dividedBy num denom
  | denom == 0 = DividedByZero
  | num == 0 = Result 0
  | (num > 0 && denom > 0) || (num < 0 && denom < 0) =
      dividedBy' (abs num) (abs denom)
  | otherwise = Result $ negate count
      where (Result count) = dividedBy' (abs num) (abs denom)

dividedBy' :: Integral a => a -> a -> DividedResult
dividedBy' num denom = go num denom 0
  where go n d count
          | n < d = Result count
          | otherwise = go (n - d) d (count + 1)

-- McCarth 91 function

mc91 :: (Num a, Ord a) => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise =
      mc91 $ mc91 $ n + 11

-- Numbers into words

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "invalid"

digits :: Int -> [Int]
digits n = go n []
    where go q ds
           | q < 10 = q : ds
           | otherwise = go (div q 10) (mod q 10 : ds)


wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
