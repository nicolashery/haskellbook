module Chapter20 where

import Data.Monoid (Product(..), Sum(..), (<>))

-- Library Functions

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . (foldMap Sum)

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . (foldMap Product)

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' y xs = foldr (\x b -> b || x == y) False xs

-- TODO (how to get starting element to compare to?)
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = undefined

-- TODO (how to get starting element to compare to?)
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = undefined

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ b -> b && False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ b -> b + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> (f a) <> b) mempty

-- Chapter Exercises

data Constant a b = Constant b

instance Foldable (Constant a) where
  foldr f z (Constant b) = f b z
  foldMap f (Constant b) = f b

data Two a b = Two a b

instance Foldable (Two a) where
  foldr f z (Two _ b) = f b z
  foldMap f (Two _ b) = f b

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z
  foldMap f (Three _ _ c) = f c

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldr f z (Three' _ b b') = f b $ f b' z
  foldMap f (Three' _ b b') = f b <> f b'

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldr f z (Four' _ b b' b'') =
    f b $ f b' $ f b'' z
  foldMap f (Four' _ b b' b'') =
    f b <> f b' <> f b''

filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool)
        -> t a
        -> f a
filterF p = foldMap g
    where g a = if p a then pure a else mempty
