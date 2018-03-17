module Chapter13 where

import Control.Monad (forever)
import Data.Char (toLower)
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let xs = cleanLine line1
  if xs == reverse xs
  then putStrLn "It's a palindrome!"
  else do
    putStrLn "Nope!"
    exitSuccess
  where cleanLine = filter isAlpha . map toLower
        isAlpha = (`elem` ['a'..'z'])

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Enter name: "
  name <- getLine
  putStr "Enter age: "
  ageInput <- getLine
  let age = read ageInput :: Age
  case mkPerson name age of
    Right person -> do
      putStrLn "Yay! Successfully got a person:"
      print person
    Left personInvalid -> do
      putStrLn "Oh no! Could not create a person:"
      print personInvalid
