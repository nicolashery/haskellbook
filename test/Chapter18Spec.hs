module Chapter18Spec where

import Chapter18 (Identity(..), List(..), Nope(..), PhhhbbtttEither(..))
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck.Classes

spec :: Spec
spec = do

  describe "Nope a" $ do
    it "should obey the Monad laws" $ hspec $ testBatch $
      monad (undefined :: Nope (Int, String, Int))

  describe "PhhhbbtttEither b a" $ do
    it "should obey the Monad laws" $ hspec $ testBatch $
      monad (undefined :: PhhhbbtttEither String (Int, String, Int))

  describe "Identity a" $ do
    it "should obey the Monad laws" $ hspec $ testBatch $
      monad (undefined :: Identity (Int, String, Int))

  describe "List a" $ do
    it "should obey the Monad laws" $ hspec $ testBatch $
      monad (undefined :: List (Int, String, Int))
