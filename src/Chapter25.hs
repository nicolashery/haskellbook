{-# LANGUAGE InstanceSigs #-}

module Chapter25 where

newtype Compose f g a =
  Compose { getCompose :: f (g a) }

instance (Functor f, Functor g) =>
          Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- Twinplicative

instance (Applicative f, Applicative g) =>
          Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose f) <*> (Compose a) =
    Compose $ (<*>) <$> f <*> a

-- Compose Instances

instance (Foldable f, Foldable g) =>
          Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) =
    foldMap (foldMap f) fga

instance (Traversable f, Traversable g) =>
          Traversable (Compose f g) where
  traverse :: Applicative h
           => (a -> h b)
           -> Compose f g a
           -> h (Compose f g b)
  traverse f (Compose fga) =
    Compose <$> traverse (traverse f) fga

-- Bifunctor

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b)
        -> (c -> d)
        -> p a c
        -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data Either' a b = Left' a | Right' b

instance Bifunctor Either' where
  bimap f _ (Left' a) = Left' (f a)
  bimap _ g (Right' b) = Right' (g b)
