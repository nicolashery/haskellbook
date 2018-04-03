module Chapter17Spec where

import Chapter17
    ( Four(..)
    , Four'(..)
    , List(..)
    , Pair(..)
    , Three(..)
    , Three'(..)
    , Two(..)
    , Validation(..)
    , ZipList'(..)
    )
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck.Classes

spec :: Spec
spec = do

  describe "List a" $ do
    it "should obey the Applicative laws" $ hspec $ testBatch $
      applicative (undefined :: List (String, String, Int))

  describe "ZipList' a" $ do
    it "should obey the Applicative laws" $ hspec $ testBatch $
      applicative (undefined :: ZipList' (String, String, Int))

  describe "Validation e a" $ do
    it "should obey the Applicative laws" $ hspec $ testBatch $
      applicative (undefined :: Validation String (String, String, Int))

  describe "Pair a" $ do
    it "should obey the Applicative laws" $ hspec $ testBatch $
      applicative (undefined :: Pair (String, String, Int))

  describe "Two a b" $ do
    it "should obey the Applicative laws" $ hspec $ testBatch $
      applicative (undefined :: Two String (String, String, Int))

  describe "Three a b c" $ do
    it "should obey the Applicative laws" $ hspec $ testBatch $
      applicative (undefined :: Three String [Int] (String, String, Int))

  describe "Three' a b" $ do
    it "should obey the Applicative laws" $ hspec $ testBatch $
      applicative (undefined :: Three' String (String, String, Int))

  describe "Four a b c d" $ do
    it "should obey the Applicative laws" $ hspec $ testBatch $
      applicative (undefined :: Four String [Int] String (String, String, Int))

  describe "Four' a b" $ do
    it "should obey the Applicative laws" $ hspec $ testBatch $
      applicative (undefined :: Four' String (String, String, Int))
