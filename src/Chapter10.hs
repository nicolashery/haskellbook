module Chapter10 where

import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)

-- Database Processing

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  , DbNumber 8001
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
  where f (DbDate utcTime) b = utcTime : b
        f _ b = b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
  where f (DbNumber i) b = i : b
        f _ b = b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr f minUtcTime
  where f (DbDate utcTime) b =
          if utcTime > b then utcTime else b
        f _ b = b
        minUtcTime = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr f 0
  where f (DbNumber i) b = i + b
        f _ b = b

avgDb :: [DatabaseItem] -> Double
avgDb db =
  let dbNumbers = filterDbNumber db
  in case length dbNumbers of
        0 -> 0
        _ -> total / count
              where total = fromIntegral $ foldr (+) 0 dbNumbers
                    count = fromIntegral $ length dbNumbers

-- Scans Exercises

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

takeFibs :: Int -> [Integer]
takeFibs n = take n fibs

smallFibs :: [Integer]
smallFibs = filter (<100) $ takeFibs 20

factorials :: [Integer]
factorials = scanl (*) 1 [2..]

takeFactorials :: Int -> [Integer]
takeFactorials n = take n factorials

-- Warm-up and review

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

fakeWords :: [String]
fakeWords = [[s, v, s'] | s <- stops, v <- vowels, s' <- stops]

nouns :: [String]
nouns = ["dog", "cat", "apple", "tree", "lake"]

verbs :: [String]
verbs = ["jump", "see", "eat", "bite", "climb"]

fakeSentences :: [String]
fakeSentences =
  [ concat [n, " ", v, " ", n']
  | n <- nouns
  , v <- verbs
  , n' <- nouns
  ]

-- Return average word length for a sentence
seekritFunc :: String -> Double
seekritFunc x = totalWordLength / wordCount
  where totalWordLength =
          fromIntegral $ sum $ map length (words x)
        wordCount =
          fromIntegral $ length (words x)

-- Rewriting functions using folds

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
-- myAny f = foldr (\a b -> f a || b) False
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
-- myElem x = foldr (\a b -> a == x || b) False
myElem x = foldr ((||) . (== x)) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = any (== x)

myReverse :: [a] -> [a]
-- myReverse = foldr (\a b -> b ++ [a]) []
myReverse = foldr (flip (++) . (: [])) []

myMap :: (a -> b) -> [a] -> [b]
-- myMap f = foldr (\a b -> f a : b) []
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
-- squishMap f = foldr (\a b -> f a ++ b) []
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs =
  foldr
  (\a b -> if f a b == GT then a else b)
  (last xs)
  xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs =
  foldr
  (\a b -> if f a b == LT then a else b)
  (last xs)
  xs
