module CipherSpec where

import Cipher (caesar, unCaesar, unVigenere, vigenere)
import Test.Hspec
import Test.QuickCheck

genSafeString :: Gen String
genSafeString = listOf $ elements (['a'..'z'] ++ ['A'..'Z'])

newtype SafeString = SafeString
  { unwrapSafeString :: String }
  deriving (Show)

instance Arbitrary SafeString where
  arbitrary = do
    s <- genSafeString
    return $ SafeString s

caesarIdentity :: Int -> SafeString -> Bool
caesarIdentity shiftBy safeMessage =
  (unCaesar shiftBy $ caesar shiftBy message) == message
  where message = unwrapSafeString safeMessage

vigenereIdentity :: SafeString -> SafeString -> Bool
vigenereIdentity safeKeyword safeMessage =
  (unVigenere keyword $ vigenere keyword message) == message
  where keyword = unwrapSafeString safeKeyword
        message = unwrapSafeString safeMessage

spec :: Spec
spec = do

  describe "caesar" $ do
    it "should return same data after encoding and decoding a string" $
      property caesarIdentity

  describe "vigenere" $ do
    it "should return same data after encoding and decoding a string" $
      property vigenereIdentity
