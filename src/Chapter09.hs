module Chapter09 where

import Data.Char (toUpper)

-- EnumFromTo

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

eft :: (Enum a, Ord a) => a -> a -> [a]
eft start stop = fromOrd $ compare start stop
  where fromOrd EQ = [start]
        fromOrd LT = go stop []
        fromOrd GT = []
        go x xs
          | x == start = x : xs
          | otherwise = go (pred x) (x : xs)

-- Thy Fearful Symmetry

breakWith :: Char -> String -> [String]
breakWith sep str = go str []
  where go [] acc = reverse acc
        go remaining acc =
            let nextPhrase = takeWhile (/= sep) remaining
                nextRemaining = dropWhile (== sep) $ dropWhile (/= sep) remaining
            in go nextRemaining (nextPhrase : acc)

myWords :: String -> [String]
myWords = breakWith ' '

myLines :: String -> [String]
myLines = breakWith '\n'

testMyLines :: IO ()
testMyLines =
  let firstSen = "Tyger Tyger, burning bright\n"
      secondSen = "In the forests of the night\n"
      thirdSen = "What immortal hand or eye\n"
      fourthSen = "Could frame thy fearful symmetry?"
      sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen
      shouldEqual =
        [ "Tyger Tyger, burning bright"
        , "In the forests of the night"
        , "What immortal hand or eye"
        , "Could frame thy fearful symmetry?"
        ]
  in print $
     "Are they equal? "
     ++ show (myLines sentences == shouldEqual)

 -- Square Cube

mySqr :: [Integer]
mySqr = [x^(2 :: Integer) | x <- [1..5]]

myCube :: [Integer]
myCube = [x^(3 :: Integer) | x <- [1..5]]

myTuples :: [(Integer, Integer)]
myTuples = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- Filtering

multThree :: (Integral a) => [a] -> [a]
multThree = filter (\x -> rem x 3 == 0)

myFilter :: String -> [String]
myFilter = filter (\x -> not $ elem x ["the", "a", "an"]) . words

-- Zipping

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c)
        -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)

-- Data.Char

capFstLetter :: String -> String
capFstLetter [] = []
capFstLetter (x:xs) = toUpper x : xs

capAllLetters :: String -> String
capAllLetters = map toUpper

fstLetterCap :: String -> Char
fstLetterCap = toUpper . head

-- Writing your own standard functions

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = (x == y) || myElem x ys

myElem' :: (Eq a) => a -> [a] -> Bool
myElem' x = any (== x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (xs:xss) = xs ++ squish xss

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMaximumBy = myExtremumBy GT

myMinimumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMinimumBy = myExtremumBy LT

myExtremumBy :: Ordering
             -> (a -> a -> Ordering)
             -> [a]
             -> a
myExtremumBy _ _ [] = error "empty list"
myExtremumBy ordering comp (x:xs) = go x xs
  where go m [] = m
        go m (x':xs') =
          go (if comp x' m == ordering then x' else m) xs'

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
