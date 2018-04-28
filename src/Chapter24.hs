module Chapter24 where

import Control.Applicative ((<|>))
import Data.List (foldl')
import Data.Ratio ((%))
import Text.Parser.Char (oneOf)
import Text.Parser.Combinators
    (count, eof, notFollowedBy, option, sepBy1, skipOptional, try)
import Text.Trifecta
    ( Parser
    , Result(..)
    , alphaNum
    , char
    , choice
    , decimal
    , digit
    , integer
    , parseString
    , some
    , string
    , unexpected
    , (<?>)
    )

-- Parsing Practice

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

one' :: Parser Char
one' = char '1' <* eof

oneTwo' :: Parser Char
oneTwo' = char '1' *> char '2' <* eof

oneTwoThree :: Parser String
oneTwoThree = choice
  [ string "123"
  , string "12"
  , string "1"
  ] <* eof

string' :: String -> Parser String
string' = mapM char

oneTwoThree' :: Parser String
oneTwoThree' = string' "123"

testParse :: Show a => Parser a -> String -> IO ()
testParse p s =
  print $ parseString p mempty s

pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)

parsingPractice :: IO ()
parsingPractice = do
  pNL "one:"
  testParse one "123"
  pNL "oneTwo:"
  testParse oneTwo "123"
  pNL "one':"
  testParse one' "123"
  pNL "oneTwo':"
  testParse oneTwo' "123"
  pNL "oneTwoThree: 1"
  testParse oneTwoThree "1"
  pNL "oneTwoThree: 12"
  testParse oneTwoThree "12"
  pNL "oneTwoThree: 123"
  testParse oneTwoThree "123"
  pNL "oneTwoThree': 123"
  testParse oneTwoThree' "123"

-- Unit of Success

unitOfSuccess :: IO ()
unitOfSuccess = do
  print $ parseString (integer <* eof) mempty "123"
  print $ parseString (integer <* eof) mempty "123abc"

-- Try Try

data FractionOrDecimal =
    Fraction Rational
  | Decimal Integer
  deriving (Eq, Show)

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseFractionOrDecimal :: Parser FractionOrDecimal
parseFractionOrDecimal =
  (Fraction <$> try parseFraction) <|> (Decimal <$> decimal)

tryTry :: IO ()
tryTry = do
  let p f = parseString f mempty
  print $ p parseFractionOrDecimal "2/3"
  print $ p parseFractionOrDecimal "123"

-- Chapter Exercises

-- Semantic Versions

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  _ <- char '.'
  minor <- decimal
  _ <- char '.'
  patch <- decimal
  release <- option [] parseRelease
  metadata <- option [] parseMetadata
  return $ SemVer major minor patch release metadata
  where
    parseRelease :: Parser Release
    parseRelease = char '-' >> parseIdentifiers

    parseMetadata :: Parser Metadata
    parseMetadata = char '+' >> parseIdentifiers

    parseIdentifiers :: Parser [NumberOrString]
    parseIdentifiers = sepBy1 numberOfString (char '.')
      where
        numberOfString = (NOSI <$> try identifierNumber) <|> (NOSS <$> identifierString)
        identifierNumber = decimal <* notFollowedBy alphaNum
        identifierString = some (alphaNum <|> char '-')

resultToMaybe :: Result a -> Maybe a
resultToMaybe (Failure _) = Nothing
resultToMaybe (Success a) = Just a

instance Ord NumberOrString where
  -- "numeric identifiers always have lower precedence
  -- than non-numeric identifiers."
  compare (NOSI _) (NOSS _) = LT
  compare (NOSS _) (NOSI _) = GT
  -- "identifiers consisting of only digits are compared numerically
  -- and identifiers with letters or hyphens are compared lexically
  -- in ASCII sort order."
  compare (NOSI n1) (NOSI n2) = compare n1 n2
  compare (NOSS s1) (NOSS s2) = compare s1 s2

instance Ord SemVer where
  -- "build metadata does not figure into precedence"
  compare v1@(SemVer major1 minor1 patch1 release1 _) v2@(SemVer major2 minor2 patch2 release2 _)
    | v1 == v2 =
        EQ
    | (major1, minor1, patch1) == (major2, minor2, patch2) =
        compareReleases release1 release2
    | otherwise =
        compare (major1, minor1, patch1) (major2, minor2, patch2)
    where
      -- "when major, minor, and patch are equal, a pre-release version
      -- has lower precedence than a normal version."
      compareReleases [] [] = EQ
      compareReleases [] _ = GT
      compareReleases _ [] = LT
      compareReleases r1 r2 = compare r1 r2

-- Integers

parseDigit :: Parser Char
parseDigit = oneOf "1234567890" <?> "parse digit"

base10Integer :: Parser Integer
base10Integer = parser <?> "integer"
  where
    parser :: Parser Integer
    parser = digitsToInteger <$> some parseDigit

digitsToInteger :: [Char] -> Integer
digitsToInteger = foldl' accum 0
  where
    accum :: Integer -> Char -> Integer
    accum n c = 10 * n + nextInt
      where
        nextInt :: Integer
        nextInt = case c of
          '0' -> 0
          '1' -> 1
          '2' -> 2
          '3' -> 3
          '4' -> 4
          '5' -> 5
          '6' -> 6
          '7' -> 7
          '8' -> 8
          '9' -> 9
          _ -> 0

base10Integer' :: Parser Integer
base10Integer' = do
  isNegative <- option False (try $ char '-' *> pure True)
  n <- base10Integer
  return $ if isNegative then negate n else n

-- Phone Numbers

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  skipOptional $ string "1-" -- US/CA country code
  skipOptional $ char '('
  area <- digitsToInt <$> count 3 digit
  skipOptional $ char ')'
  skipOptional $ char ' '
  skipOptional $ char '-'
  exchange <- digitsToInt <$> count 3 digit
  skipOptional $ char '-'
  line <- digitsToInt <$> count 4 digit
  return $ PhoneNumber area exchange line
  where
    digitsToInt :: [Char] -> Int
    digitsToInt = fromInteger . digitsToInteger

-- TODO: Log File

-- TODO: IP Addresses

-- TODO: DOT Language
