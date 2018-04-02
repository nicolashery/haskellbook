module Chapter16Spec where

import Chapter16
    ( Four(..)
    , Four'(..)
    , Identity(..)
    , Pair(..)
    , Three(..)
    , Three'(..)
    , Two(..)
    )
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) =>
                  f a
               -> Fun a b
               -> Fun b c
               -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

spec :: Spec
spec = do

  describe "Identity a" $ do
    it "should obey the Functor identity law" $ property $
      (functorIdentity :: Identity Int -> Bool)

    it "should obey the Functor composition law" $ property $
      (functorCompose :: Identity Int -> Fun Int String -> Fun String Int -> Bool)

  describe "Pair a" $ do
    it "should obey the Functor identity law" $ property $
      (functorIdentity :: Pair Int -> Bool)

    it "should obey the Functor composition law" $ property $
      (functorCompose :: Pair Int -> Fun Int String -> Fun String Int -> Bool)

  describe "Two a b" $ do
    it "should obey the Functor identity law" $ property $
      (functorIdentity :: Two String Int -> Bool)

    it "should obey the Functor composition law" $ property $
      (functorCompose :: Two String Int -> Fun Int String -> Fun String Int -> Bool)

  describe "Three a b c" $ do
    it "should obey the Functor identity law" $ property $
      (functorIdentity :: Three String Double Int -> Bool)

    it "should obey the Functor composition law" $ property $
      (functorCompose :: Three String Double Int -> Fun Int String -> Fun String Int -> Bool)

  describe "Three' a b" $ do
    it "should obey the Functor identity law" $ property $
      (functorIdentity :: Three' String Int -> Bool)

    it "should obey the Functor composition law" $ property $
      (functorCompose :: Three' String Int -> Fun Int String -> Fun String Int -> Bool)

  describe "Four a b c d" $ do
    it "should obey the Functor identity law" $ property $
      (functorIdentity :: Four String Double Bool Int -> Bool)

    it "should obey the Functor composition law" $ property $
      (functorCompose :: Four String Double Bool Int -> Fun Int String -> Fun String Int -> Bool)

  describe "Four' a b" $ do
    it "should obey the Functor identity law" $ property $
      (functorIdentity :: Four' String Int -> Bool)

    it "should obey the Functor composition law" $ property $
      (functorCompose :: Four' String Int -> Fun Int String -> Fun String Int -> Bool)
