module Chapter15Spec where

import Chapter15
    ( BoolConj
    , BoolDisj
    , Combine(..)
    , Comp(..)
    , First'(..)
    , Four(..)
    , Identity(..)
    , Mem(..)
    , Optional(..)
    , Or(..)
    , Three(..)
    , Trivial(..)
    , Two(..)
    , Validation(..)
    )
import Data.Monoid ((<>))
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup as SG
import Test.Hspec
import Test.QuickCheck hiding (Failure, Success)

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a SG.<> (b SG.<> c)) == ((a SG.<> b) SG.<> c)

type FirstMappend
  = First' String
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

type IdAssoc =
  Identity String -> Identity String -> Identity String -> Bool

type TwoAssoc
  = Two String (SG.Sum Int)
  -> Two String (SG.Sum Int)
  -> Two String (SG.Sum Int)
  -> Bool

type TwoId
  = Two String (SG.Sum Int)
  -> Bool

type ThreeAssoc
  = Three String (SG.Sum Int) (SG.Product Int)
  -> Three String (SG.Sum Int) (SG.Product Int)
  -> Three String (SG.Sum Int) (SG.Product Int)
  -> Bool

type FourAssoc
  = Four String (SG.Sum Int) (SG.Product Int) SG.All
  -> Four String (SG.Sum Int) (SG.Product Int) SG.All
  -> Four String (SG.Sum Int) (SG.Product Int) SG.All
  -> Bool

type BoolConjAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool

type BoolDisjAssoc =
  BoolDisj -> BoolDisj -> BoolDisj -> Bool

type OrAssoc
  = Or Int String
  -> Or Int String
  -> Or Int String
  -> Bool

combineSemigroupAssoc
  :: (Eq b, Semigroup b)
  => Combine a b
  -> Combine a b
  -> Combine a b
  -> a
  -> Bool
combineSemigroupAssoc f g h a =
  (unCombine (f SG.<> (g SG.<> h)) $ a) == (unCombine ((f SG.<> g) SG.<> h) $ a)

combineLeftIdentity
  :: (Eq b, Semigroup b, Monoid b)
  => Combine a b
  -> a
  -> Bool
combineLeftIdentity f a =
  (unCombine (mempty <> f) $ a) == (unCombine f $ a)

combineRightIdentity
  :: (Eq b, Semigroup b, Monoid b)
  => Combine a b
  -> a
  -> Bool
combineRightIdentity f a =
  (unCombine (f <> mempty) $ a) == (unCombine f $ a)

type CombineAssoc
  = Combine Int (SG.Sum Int)
  -> Combine Int (SG.Sum Int)
  -> Combine Int (SG.Sum Int)
  -> Int
  -> Bool

type CombineId
  = Combine Int (SG.Sum Int)
  -> Int
  -> Bool

compSemigroupAssoc
  :: (Eq a)
  => Comp a
  -> Comp a
  -> Comp a
  -> a
  -> Bool
compSemigroupAssoc f g h a =
  (unComp (f SG.<> (g SG.<> h)) $ a) == (unComp ((f SG.<> g) SG.<> h) $ a)

compLeftIdentity :: (Eq a) => Comp a -> a -> Bool
compLeftIdentity f a =
  (unComp (mempty <> f) $ a) == (unComp f $ a)

compRightIdentity :: (Eq a) => Comp a -> a -> Bool
compRightIdentity f a =
  (unComp (f <> mempty) $ a) == (unComp f $ a)

type CompAssoc
  = Comp Int
  -> Comp Int
  -> Comp Int
  -> Int
  -> Bool

type CompId
  = Comp Int
  -> Int
  -> Bool

type ValidationAssoc
  = Validation String Int
  -> Validation String Int
  -> Validation String Int
  -> Bool

spec :: Spec
spec = do

  describe "First' a" $ do
    it "returns the leftmost non-Nada value" $ do
      let values = map First' $ [Nada, Only 1, Only 2, Nada, Only 3]
      (getFirst' $ mconcat values) `shouldBe` (Only 1 :: Optional Int)

    it "should obey the Monoid associativity law" $ property $
      (monoidAssoc :: FirstMappend)

    it "should obey the Monoid left identity law" $ property $
      (monoidLeftIdentity :: FstId)

    it "should obey the Monoid right identity law" $ property $
      (monoidRightIdentity :: FstId)

  describe "Trivial" $ do
    it "should obey the Semigroup associativity law" $ property $
      (semigroupAssoc :: TrivAssoc)

    it "should obey the Monoid left identity law" $ property $
      (monoidLeftIdentity :: Trivial -> Bool)

    it "should obey the Monoid right identity law" $ property $
      (monoidRightIdentity :: Trivial -> Bool)

  describe "Identity a" $ do
    it "should obey the Semigroup associativity law" $ property $
      (semigroupAssoc :: IdAssoc)

    it "should obey the Monoid left identity law" $ property $
      (monoidLeftIdentity :: Identity String -> Bool)

    it "should obey the Monoid right identity law" $ property $
      (monoidRightIdentity :: Identity String -> Bool)

  describe "Two a b" $ do
    it "should obey the Semigroup associativity law" $ property $
      (semigroupAssoc :: TwoAssoc)

    it "should obey the Monoid left identity law" $ property $
      (monoidLeftIdentity :: TwoId)

    it "should obey the Monoid right identity law" $ property $
      (monoidRightIdentity :: TwoId)

  describe "Three a b c" $ do
    it "should obey the Semigroup associativity law" $ property $
      (semigroupAssoc :: ThreeAssoc)

  describe "Four a b c d" $ do
    it "should obey the Semigroup associativity law" $ property $
      (semigroupAssoc :: FourAssoc)

  describe "BoolConj" $ do
    it "should obey the Semigroup associativity law" $ property $
      (semigroupAssoc :: BoolConjAssoc)

    it "should obey the Monoid left identity law" $ property $
      (monoidLeftIdentity :: BoolConj -> Bool)

    it "should obey the Monoid right identity law" $ property $
      (monoidRightIdentity :: BoolConj -> Bool)

  describe "BoolDisj" $ do
    it "should obey the Semigroup associativity law" $ property $
      (semigroupAssoc :: BoolDisjAssoc)

    it "should obey the Monoid left identity law" $ property $
      (monoidLeftIdentity :: BoolDisj -> Bool)

    it "should obey the Monoid right identity law" $ property $
      (monoidRightIdentity :: BoolDisj -> Bool)

  describe "Or a b" $ do
    it "returns the first Snd value" $ do
      (Fst 1 SG.<> Snd 2 SG.<> Snd 3) `shouldBe` (Snd 2 :: Or Int Int)

    it "returns the last Fst value if no Snd value" $ do
      (Fst 1 SG.<> Fst 2 SG.<> Fst 3) `shouldBe` (Fst 3 :: Or Int Int)

    it "should obey the Semigroup associativity law" $ property $
      (semigroupAssoc :: OrAssoc)

  describe "Combine a b" $ do
    it "behaves as expected" $ do
      let f = (Combine $ \n -> SG.Sum (n + 1)) :: Combine Int (SG.Sum Int)
      let g = (Combine $ \n -> SG.Sum (n - 1)) :: Combine Int (SG.Sum Int)
      (unCombine (f SG.<> g) $ 0) `shouldBe` (SG.Sum 0)
      (unCombine (f SG.<> g) $ 1) `shouldBe` (SG.Sum 2)
      (unCombine (f SG.<> f) $ 1) `shouldBe` (SG.Sum 4)
      (unCombine (g SG.<> f) $ 1) `shouldBe` (SG.Sum 2)

    it "should obey the Semigroup associativity law" $ property $
      (combineSemigroupAssoc :: CombineAssoc)

    it "should obey the Monoid left identity law" $ property $
      (combineLeftIdentity :: CombineId)

    it "should obey the Monoid right identity law" $ property $
      (combineRightIdentity :: CombineId)

  describe "Comp a" $ do
    it "should obey the Semigroup associativity law" $ property $
      (compSemigroupAssoc :: CompAssoc)

    it "should obey the Monoid left identity law" $ property $
      (compLeftIdentity :: CompId)

    it "should obey the Monoid right identity law" $ property $
      (compRightIdentity :: CompId)

  describe "Validation a b" $ do
    it "behaves as expected" $ do
      let failure = Failure :: String -> Validation String Int
      let success = Success :: Int -> Validation String Int
      (success 1 SG.<> failure "blah") `shouldBe` (Success 1)
      (failure "woot" SG.<> failure "blah") `shouldBe` (Failure "wootblah")
      (success 1 SG.<> success 2) `shouldBe` (Success 1)
      (failure "woot" SG.<> success 2) `shouldBe` (Success 2)

    it "should obey the Semigroup associativity law" $ property $
      (semigroupAssoc :: ValidationAssoc)

  describe "Mem s a" $ do
    it "behaves as expected" $ do
      let f' :: Mem Int String
          f' = Mem $ \s -> ("hi", s + 1)
          rmzero = runMem mempty 0
          rmleft = runMem (f' <> mempty) 0
          rmright = runMem (mempty <> f') 0
      rmleft `shouldBe` ("hi", 1)
      rmright `shouldBe` ("hi", 1)
      rmzero `shouldBe` (("", 0) :: (String, Int))
      rmleft `shouldBe` (runMem f' 0)
      rmright `shouldBe` (runMem f' 0)
