module Chapter15 where

import Data.Monoid ((<>))
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup as SG
import Test.QuickCheck (Arbitrary(..), CoArbitrary, frequency, oneof)

-- Optional Monoid

data Optional a = Nada | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend a Nada = a
  mappend Nada b = b
  mappend (Only x) (Only y) = Only (x `mappend` y)

-- Madness

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj = mconcat
  [ e
  , "! he said "
  , adv
  , " as he jumped into his car "
  , noun
  , " and drove off with his "
  , adj
  , " wife."
  ]

-- Maybe Another Monoid

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) (First' Nada) = First' Nada
  mappend (First' Nada) (First' (Only y)) = First' $ Only y
  mappend (First' (Only x)) _ = First' $ Only x

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return $ First' Nada),
               (3, return $ First' (Only a))]

-- Semigroup & Monoid

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (SG.<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a SG.<> b)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (SG.<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a SG.<> a') (b SG.<> b')

instance
    ( Semigroup a
    , Monoid a
    , Semigroup b
    , Monoid b
    ) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (SG.<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a SG.<> a') (b SG.<> b') (c SG.<> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a SG.<> a') (b SG.<> b') (c SG.<> c') (d SG.<> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj a) <> (BoolConj b) = BoolConj (a && b)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (SG.<>)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj a) <> (BoolDisj b) = BoolDisj (a || b)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (SG.<>)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

data Or a b = Fst a | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Fst _) <> (Fst a) = Fst a
  (Fst _) <> (Snd b) = Snd b
  (Snd b) <> _ = Snd b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Fst a, return $ Snd b]

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  -- Don't know if there is a better way
  show _ = "Combine { unCombine :: (a -> b) }"

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \a -> f a SG.<> g a

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine (const mempty)
  mappend = (SG.<>)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Show (Comp a) where
  -- Don't know if there is a better way
  show _ = "Comp { unComp :: (a -> a) }"

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance Monoid (Comp a) where
  mempty = Comp id
  mappend = (SG.<>)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return $ Comp f

data Validation a b = Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Failure a) <> (Failure a') = Failure (a SG.<> a')
  (Failure _) <> (Success b) = Success b
  (Success b) <> _ = Success b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Failure a),
               (3, return $ Success b)]

newtype Mem s a =
  Mem { runMem :: s -> (a,s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  (Mem f) `mappend` (Mem g) = Mem h
    where h s = (af <> ag, s'')
              where (af, s') = f s
                    (ag, s'') = g s'
