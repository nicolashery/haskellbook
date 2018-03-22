module Chapter11Spec where

import Chapter11 (capitalizeWord)
import Test.Hspec
import Test.QuickCheck

twice :: (a -> a) -> (a -> a)
twice f = f . f

fourTimes :: (a -> a) -> (a -> a)
fourTimes = twice . twice

capitalizeWordIndempotence :: String -> Bool
capitalizeWordIndempotence x =
  (capitalizeWord x == twice capitalizeWord x)
  &&
  (capitalizeWord x == fourTimes capitalizeWord x)

spec :: Spec
spec = do

  describe "capitalizeWord" $ do
    it "is indempotent" $ property capitalizeWordIndempotence
