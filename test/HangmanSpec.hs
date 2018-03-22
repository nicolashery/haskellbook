module HangmanSpec where

import Hangman (Puzzle(..), fillInCharacter, handleGuess)
import Test.Hspec

spec :: Spec
spec = do

  describe "fillInCharacter" $ do

    it "should fill in correctly guessed character" $ do
      let puzzle = Puzzle { word = "foo"
                          , discovered = [Nothing, Nothing, Nothing]
                          , guessed = [] }
      let puzzle' = Puzzle { word = "foo"
                           , discovered = [Nothing, Just 'o', Just 'o']
                           , guessed = ['o'] }
      fillInCharacter puzzle 'o' `shouldBe` puzzle'

    it "should handle an incorrect guess" $ do
      let puzzle = Puzzle { word = "foo"
                          , discovered = [Nothing, Just 'o', Just 'o']
                          , guessed = ['o'] }
      let puzzle' = Puzzle { word = "foo"
                           , discovered = [Nothing, Just 'o', Just 'o']
                           , guessed = ['p', 'o'] }
      fillInCharacter puzzle 'p' `shouldBe` puzzle'

    describe "handleGuess" $ do

      it "should return same puzzle if guessing same character again" $ do
        let puzzle = Puzzle { word = "foo"
                            , discovered = [Nothing, Just 'o', Just 'o']
                            , guessed = ['o'] }
        result <- handleGuess puzzle 'o'
        result `shouldBe` puzzle

      it "should fill in correctly guessed character" $ do
        let puzzle = Puzzle { word = "foo"
                            , discovered = [Nothing, Nothing, Nothing]
                            , guessed = [] }
        let puzzle' = Puzzle { word = "foo"
                             , discovered = [Nothing, Just 'o', Just 'o']
                             , guessed = ['o'] }
        result <- handleGuess puzzle 'o'
        result `shouldBe` puzzle'

      it "should handle an incorrect guess" $ do
        let puzzle = Puzzle { word = "foo"
                            , discovered = [Nothing, Just 'o', Just 'o']
                            , guessed = ['o'] }
        let puzzle' = Puzzle { word = "foo"
                             , discovered = [Nothing, Just 'o', Just 'o']
                             , guessed = ['p', 'o'] }
        result <- handleGuess puzzle 'p'
        result `shouldBe` puzzle'
