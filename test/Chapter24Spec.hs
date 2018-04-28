module Chapter24Spec where

import Chapter24
    ( NumberOrString(..)
    , PhoneNumber(..)
    , SemVer(..)
    , parsePhone
    , parseSemVer
    , resultToMaybe
    )
import Test.Hspec
import Text.Trifecta (parseString)

spec :: Spec
spec = do

  describe "parseSemVer" $ do
    it "should correctly parse 2.1.1" $ do
      let semVer = resultToMaybe $ parseString parseSemVer mempty "2.1.1"
      semVer `shouldBe` Just (SemVer 2 1 1 [] [])

    it "should correctly parse 1.0.0-x.7.z.92" $ do
      let semVer = resultToMaybe $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
      semVer `shouldBe` Just (SemVer 1 0 0
                                 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])

    it "should correctly parse 1.0.0-beta+exp-sha.5114f85" $ do
      let semVer = resultToMaybe $ parseString parseSemVer mempty "1.0.0-beta+exp-sha.5114f85"
      semVer `shouldBe` Just (SemVer 1 0 0
                                  [NOSS "beta"]
                                  [NOSS "exp-sha", NOSS "5114f85"])

  describe "Ord SemVer" $ do
    let v = resultToMaybe . parseString parseSemVer mempty

    it "should return correct precendence for major, minor, and patch versions" $ do
      v "1.0.0" < v "2.0.0" `shouldBe` True
      v "2.0.0" < v "2.1.0" `shouldBe` True
      v "2.1.0" < v "2.1.1" `shouldBe` True

    it "should return correct precendence for release versions" $ do
      v "1.0.0-alpha" < v "1.0.0-alpha.1" `shouldBe` True
      v "1.0.0-alpha.beta" < v "1.0.0-beta" `shouldBe` True
      v "1.0.0-beta" < v "1.0.0-beta.2" `shouldBe` True
      v "1.0.0-beta.2" < v "1.0.0-beta.11" `shouldBe` True
      v "1.0.0-beta.11" < v "1.0.0-rc.1" `shouldBe` True
      v "1.0.0-rc.1" < v "1.0.0" `shouldBe` True

  describe "parsePhone" $ do
    let p = resultToMaybe . parseString parsePhone mempty

    it "should correctly parse 123-456-7890" $
      p "123-456-7890" `shouldBe` Just (PhoneNumber 123 456 7890)

    it "should correctly parse 1234567890" $
      p "1234567890" `shouldBe` Just (PhoneNumber 123 456 7890)

    it "should correctly parse (123) 456-7890" $
      p "(123) 456-7890" `shouldBe` Just (PhoneNumber 123 456 7890)

    it "should correctly parse 1-123-456-7890" $
      p "1-123-456-7890" `shouldBe` Just (PhoneNumber 123 456 7890)
