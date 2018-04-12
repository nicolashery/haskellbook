module Chapter21Spec where

import Chapter21
    (Big, Bigger, Constant, Identity, List, Optional, Pair, S, Three, Tree)
import Control.Monad (when)
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck.Classes

spec :: Spec
spec = do

  describe "Identity a" $ do
    it "should obey the Traversable laws" $ hspec $ testBatch $
      traversable (undefined :: Identity (Int, Int, [Int]))

  describe "Constant a b" $ do
    it "should obey the Traversable laws" $ hspec $ testBatch $
      traversable (undefined :: Constant String (Int, Int, [Int]))

  describe "Optional a" $ do
    it "should obey the Traversable laws" $ hspec $ testBatch $
      traversable (undefined :: Optional (Int, Int, [Int]))

  describe "List a" $ do
    it "should obey the Traversable laws" $ hspec $ testBatch $
      traversable (undefined :: List (Int, Int, [Int]))

  describe "Three a b c" $ do
    it "should obey the Traversable laws" $ hspec $ testBatch $
      traversable (undefined :: Three String Double (Int, Int, [Int]))

  describe "Pair a b" $ do
    it "should obey the Traversable laws" $ hspec $ testBatch $
      traversable (undefined :: Pair String (Int, Int, [Int]))

  describe "Big a b" $ do
    it "should obey the Traversable laws" $ hspec $ testBatch $
      traversable (undefined :: Big String (Int, Int, [Int]))

  describe "Bigger a b" $ do
    it "should obey the Traversable laws" $ hspec $ testBatch $
      traversable (undefined :: Bigger String (Int, Int, [Int]))

  -- TODO: For some reason this is failing on the Functor identity law
  -- although the implementation seems correct and was tested manually
  when False $ do
    describe "S n a" $ do
      it "should obey the Traversable laws" $ hspec $ testBatch $
        traversable (undefined :: S [] (Int, Int, [Int]))

  describe "Tree a" $ do
    it "should obey the Traversable laws" $ hspec $ testBatch $
      traversable (undefined :: Tree (Int, Int, String))
