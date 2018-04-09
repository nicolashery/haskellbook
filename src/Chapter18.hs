module Chapter18 where

import Control.Monad (join, liftM, liftM2)
import Test.QuickCheck (Arbitrary(..), frequency, oneof)
import Test.QuickCheck.Checkers (EqProp(..), eq)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

-- Either Monad

data Sum a b = First a | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  (First a) <*> _ = First a
  _ <*> (First a) = First a
  (Second f) <*> (Second b) = Second (f b)

instance Monad (Sum a) where
  return = pure
  (First a) >>= _ = First a
  (Second b) >>= f = f b

-- Chapter Exercises: Monad Instances

data Nope a = NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

data PhhhbbtttEither b a =
    Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' (f a)
  fmap _ (Right' b) = Right' b

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  (Right' b) <*> _ = Right' b
  _ <*> (Right' b) = Right' b
  (Left' f) <*> (Left' a) = Left' (f a)

instance Monad (PhhhbbtttEither b) where
  return = pure
  (Left' a) >>= f = f a
  (Right' b) >>= _ = Right' b

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary =
    oneof [Left' <$> arbitrary, Right' <$> arbitrary]

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  fs <*> xs = flatMap (\f -> fmap f xs) fs

instance Monad List where
  return a = Cons a Nil
  Nil >>= _ = Nil
  (Cons x xs) >>= f =
    (f x) `append` (xs >>= f)

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
  arbitrary =
    frequency [(1, return Nil),
               (2, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- Chapter Exercises: Functions using Monad and Functor

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = liftM

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

ap :: Monad m => m a -> m (a -> b) -> m b
ap = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f =  fmap (:) (f x) <*> meh xs f

flipType :: (Monad m) => [m a] -> m [a]
flipType = (flip meh) (join . return)
