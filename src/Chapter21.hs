{-# LANGUAGE FlexibleContexts #-}

module Chapter21 where

import Data.Monoid ((<>))
import Test.QuickCheck (Arbitrary(..), Property, Testable, frequency, property)
import Test.QuickCheck.Checkers (EqProp(..), eq, (.&.))

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldr f z (Identity a) = f a z
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldr _ z _ = z
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure (Constant a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq

data Optional a = Nada | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep a) = f a z
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary =
    frequency [(1, return Nada),
               (2, Yep <$> arbitrary)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldr _ z Nil = z
  foldr f z (Cons x xs) = f x (foldr f z xs)
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = (f x) <> (foldMap f xs)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency [(1, return Nil),
               (2, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Pair a b = Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldr f z (Pair _ b) = f b z
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

data Big a b = Big a b b
  deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldr f z (Big _ b b') = f b $ f b' z
  foldMap f (Big _ b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

data Bigger a b = Bigger a b b b
  deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldr f z (Bigger _ b b' b'') = f b $ f b' $ f b'' z
  foldMap f (Bigger _ b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

data S n a = S (n a) a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance Foldable n => Foldable (S n) where
  foldr f z (S na a) = foldr f (f a z) na
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), EqProp a) => EqProp (S n a) where
  (S x y) =-= (S p q) =
    (property $ (=-=) <$> x <*> p) .&. (y =-= q)

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node r a l) = Node (fmap f r) (f a) (fmap f l)

instance Foldable Tree where
  foldr _ z Empty = z
  foldr f z (Leaf a) = f a z
  foldr f z (Node r a l) = foldr f (foldr f (f a z) r) l

  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node r a l) = foldMap f r <> f a <> foldMap f l

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node r a l) = Node <$> traverse f r <*> f a <*> traverse f l

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    frequency [(1, return Empty),
               (2, Leaf <$> arbitrary),
               (2, Node <$> arbitrary <*> arbitrary <*> arbitrary)]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq
