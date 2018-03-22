module Chapter14Spec where

import Chapter14 (half)
import Control.Monad (when)
import Data.List (sort)
import Test.Hspec
import Test.QuickCheck

halfIdentity :: (Fractional a) => a -> a
halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y =
  x + y == y + x

timesAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
timesAssociative x y z =
  x * (y * z) == (x * y) * z

timesCommutative :: (Eq a, Num a) => a -> a -> Bool
timesCommutative x y =
  x * y == y * x

quotRemLaw :: (Integral a) => a -> a -> Bool
quotRemLaw _ 0 = True -- skip division by 0
quotRemLaw x y =
  (quot x y) * y + (rem x y) == x

divModLaw :: (Integral a) => a -> a -> Bool
divModLaw _ 0 = True -- skip division by 0
divModLaw x y =
  (div x y) * y + (mod x y) == x

powerAssociative :: (Eq a, Integral a) => a -> a -> a -> Bool
powerAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z

powerCommutative :: (Eq a, Integral a) => a -> a -> Bool
powerCommutative x y =
  x ^ y == y ^ x

compositionProperty :: (Eq c) => (b -> c) -> (a -> b) -> a -> Bool
compositionProperty f g x =
  (f $ g x) == ((f . g) x)

squareIdentity :: (Eq a, Floating a) => a -> Bool
squareIdentity x =
  (square . sqrt) x == x
  where square y = y * y

twice :: (a -> a) -> (a -> a)
twice f = f . f

fourTimes :: (a -> a) -> (a -> a)
fourTimes = twice . twice

sortIndempotence :: (Ord a) => [a] -> Bool
sortIndempotence xs =
  (sort xs == twice sort xs)
  &&
  (sort xs == fourTimes sort xs)

data Fool = Fulse | Frue
  deriving (Eq, Show)

foolGenEqual :: Gen Fool
foolGenEqual =
  oneof [ return Fulse
        , return Frue
        ]

foolGenFulsePls :: Gen Fool
foolGenFulsePls =
  frequency [ (2, return Fulse)
            , (1, return Frue)
            ]

spec :: Spec
spec = do

  describe "half" $ do
    it "is inverse to (*2)" $ property $
      \x -> halfIdentity x == (x :: Double)

  describe "sort" $ do
    it "returns a sorted list" $ property $
      \xs -> listOrdered $ sort (xs :: [Int])
    it "is indempotent" $ property $
     (sortIndempotence :: [Int] -> Bool)

  describe "addition" $ do
    it "is associative" $ property $
      (plusAssociative :: Int -> Int -> Int -> Bool)
    it "is commutative" $ property $
      (plusCommutative :: Int -> Int -> Bool)

  describe "multiplication" $ do
    it "is associative" $ property $
      (timesAssociative :: Int -> Int -> Int -> Bool)
    it "is commutative" $ property $
      (timesCommutative :: Int -> Int -> Bool)

  describe "Euclidean division" $ do
    it "obeys law for quot and rem" $ property $
      (quotRemLaw :: Int -> Int -> Bool)
    it "obeys law for div and mod" $ property $
      (divModLaw :: Int -> Int -> Bool)

  -- Switch to `when True` to check that `(^)`
  -- is not associative nor commutative
  when False $ do
    describe "exponent" $ do
      it "is associative" $ property $
        (powerAssociative :: Int -> Int -> Int -> Bool)
      it "is commutative" $ property $
        (powerCommutative :: Int -> Int -> Bool)

  describe "reverse" $ do
    it "is same as identity when reversing twice" $ property $
      \xs -> (reverse $ reverse xs) == (id xs :: [Int])

  describe "composition" $ do
    it "obeys relation between ($) and (.)" $ property $
      (compositionProperty (*2) (+3) :: Int -> Bool)

  describe "foldr" $ do
    -- Switch to `when True` to check that the
    -- following property does not hold
    when False $ do
      it "is equivalent to (++) when applied to (:)" $ property $
        ((\xs ys -> foldr (:) xs ys == xs ++ ys) :: [Int] -> [Int] -> Bool)
    it "is equivalent to concat when applied to (++) []" $ property $
      ((\xs -> foldr (++) [] xs == concat xs) :: [String] -> Bool)

  describe "take" $ do
    -- Switch to `when True` to check that the
    -- following property does not hold
    when False $ do
      it "returns a list that is always of length n" $ property $
        ((\n xs -> length (take n xs) == n) :: Int -> [Int] -> Bool)

  describe "read and show" $ do
    it "completes a round trip" $ property $
      ((\x -> (read (show x)) == x) :: Int -> Bool)

  describe "sqrt" $ do
    -- Switch to `when True` to check that the
    -- following property does not hold
    when False $ do
      it "forms an identity with square" $ property $
        (squareIdentity :: Double -> Bool)
