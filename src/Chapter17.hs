module Chapter17 where

import Control.Applicative (liftA3)
import Data.List (elemIndex)
import Data.Monoid ((<>))
import Test.QuickCheck (Arbitrary(..), frequency, oneof)
import Test.QuickCheck.Checkers (EqProp(..), eq)

-- Lookups

added :: Maybe Integer
added =
  (+3) <$> (lookup (3 :: Integer) $ zip [1, 2, 3] [4, 5, 6])

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z
  where
    y :: Maybe Integer
    y = lookup (3 :: Integer) $ zip [1, 2, 3] [4, 5, 6]

    z :: Maybe Integer
    z = lookup (2 :: Integer) $ zip [1, 2, 3] [4, 5, 6]

maxed :: Maybe Int
maxed = max' <$> x <*> y
  where
    x :: Maybe Int
    x = elemIndex (3 :: Int) [1, 2, 3, 4, 5]

    y :: Maybe Int
    y = elemIndex (4 :: Int) [1, 2, 3, 4, 5]

    max' :: Int -> Int -> Int
    max' = max

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x <*> y
  where
    xs = [1, 2, 3]
    ys = [4, 5, 6]

    x :: Maybe Integer
    x = lookup (3 :: Integer) $ zip xs ys

    y :: Maybe Integer
    y = lookup (2 :: Integer) $ zip xs ys

-- Identity Instance

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

-- Constance Instance

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  _ <*> (Constant a) = Constant a

-- Fixer Upper

fixerUpper1 :: Maybe String
fixerUpper1 = const <$> Just "Hello" <*> (pure "World")

fixerUpper2 :: Maybe (Int, Int, String, [Int])
fixerUpper2 =
  (,,,) <$> Just 90
        <*> Just 10
        <*> Just "Tierness"
        <*> pure [1, 2, 3]

-- List Applicative

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil  = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  fs <*> xs = flatMap (\f -> fmap f xs) fs

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return Nil),
               (2, pure (Cons a) <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

toMyList :: [a] -> List a
toMyList = foldr Cons Nil

-- ZipList Applicative

newtype ZipList' a = ZipList' (List a)
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = ZipList' . pure
  ZipList' Nil <*> _ = ZipList' Nil
  _ <*> ZipList' Nil = ZipList' Nil
  (ZipList' (Cons f Nil)) <*> (ZipList' xs) =
    ZipList' $ zipWith' ($) (repeat' f) xs
  (ZipList' fs) <*> (ZipList' (Cons x Nil)) =
    ZipList' $ zipWith' ($) fs (repeat' x)
  (ZipList' fs) <*> (ZipList' xs) =
    ZipList' $ zipWith' ($) fs xs

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' _ Nil _ = Nil
zipWith' _ _ Nil = Nil
zipWith' f (Cons x xs) (Cons y ys) =
  Cons (f x y) (zipWith' f xs ys)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

testZipList :: IO ()
testZipList = do
  let zl' = ZipList'
  let z = zl' $ toMyList ([(+9), (*2), (+8)])
  let z' = zl' $ toMyList [1..3]
  print $ (z <*> z' :: ZipList' Int)
  let z'' = zl' $ toMyList (repeat 1)
  print $ (z <*> z'' :: ZipList' Int)
  print $ (pure id <*> z' :: ZipList' Int)

-- Validation Applicative

data Validation e a = Failure e | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (Failure e) <*> (Success _) = Failure e
  (Success _) <*> (Failure e) = Failure e
  (Success f) <*> (Success a) = Success (f a)
  (Failure e) <*> (Failure e') = Failure (e <> e')

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [Failure <$> arbitrary, Success <$> arbitrary]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

-- Chapter Exercises

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (Pair f f') <*> (Pair a a') = Pair (f a) (f' a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (Two a f) <*> (Two a' b) = Two (a <> a') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three a b f) <*> (Three a' b' c) = Three (a <> a') (b <> b') (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (Three' a f f') <*> (Three' a' b b') = Three' (a <> a') (f b) (f' b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (Four a b c f) <*> (Four a' b' c' d) =
    Four (a <> a') (b <> b') (c <> c') (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary =
    Four <$> arbitrary
         <*> arbitrary
         <*> arbitrary
         <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' a1 a2 a3 f) <*> (Four' a1' a2' a3' b) =
    Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary =
    Four' <$> arbitrary
         <*> arbitrary
         <*> arbitrary
         <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

-- Combinations

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

fakeWords :: [String]
fakeWords =
  fmap (\(s, v, s') -> [s, v, s']) (combos stops vowels stops)
