module Chapter28Spec where

import Chapter28
    ( Queue
    , append
    , cons
    , empty
    , emptyQueue
    , popList
    , popQueue
    , popSequence
    , pushList
    , pushQueue
    , pushSequence
    , queueFromList
    , singleton
    , snoc
    , toList
    )
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import Test.Hspec

spec :: Spec
spec = do

  describe "DList" $ do
    it "should create empty list" $ do
      let result = toList empty
      result `shouldBe` ([] :: [Int])

    it "should create singleton list" $ do
      let result = toList $ singleton 1
      result `shouldBe` ([1] :: [Int])

    it "should create list with cons" $ do
      let result = toList $ 1 `cons` 2 `cons` 3 `cons` empty
      result `shouldBe` ([1, 2, 3] :: [Int])

    it "should create list with snoc" $ do
      let result = toList $ empty `snoc` 1 `snoc` 2 `snoc` 3
      result `shouldBe` ([1, 2, 3] :: [Int])

    it "should append two dlists" $ do
      let xs = 1 `cons` 2 `cons` empty
          ys = 3 `cons` 4 `cons` empty
          result = toList $ xs `append` ys
      result `shouldBe` ([1, 2, 3, 4] :: [Int])

  describe "Queue" $ do
    it "should respect FIFO for implementation with List" $ do
      let queue :: [Int]
          queue = pushList 3 $ pushList 2 $ pushList 1 []
      popList queue `shouldBe` Just (1, [2, 3])

    it "should respect FIFO for implementation with Sequence" $ do
      let queue :: Seq Int
          queue = pushSequence 3 $ pushSequence 2 $ pushSequence 1 SQ.empty
      popSequence queue `shouldBe` Just (1, SQ.fromList [2, 3])

    it "should respect FIFO for implementation with Queue" $ do
      let queue :: Queue Int
          queue = pushQueue 3 $ pushQueue 2 $ pushQueue 1 emptyQueue
      popQueue queue `shouldBe` Just (1, queueFromList [2, 3])
