module Chapter06 where

import Data.List (sort)

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (TisAn x) == (TisAn x') = x == x'

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (Two x y) == (Two x' y') = x == x' && y == y'

data StringOrInt =
  TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (TisAnInt x) == (TisAnInt x') = x == x'
  (TisAString str) == (TisAString str') = str == str'
  _ == _ = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (Pair x y) == (Pair x' y') = x == x' && y == y'

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (Tuple x y) == (Tuple x' y') = x == x' && y == y'

data Which a =
  ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (ThisOne x) == (ThisOne x') = x == x'
  (ThatOne x) == (ThatOne x') = x == x'
  _ == _ = False

data EitherOr a b =
  Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (Hello x) == (Hello x') = x == x'
  (Goodbye y) == (Goodbye y') = y == y'
  _ == _ = False

-- Given a datatype declaration, what can we do?
-------------------------

data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah | Woot deriving Show

settleDown :: Mood -> Mood
settleDown Woot = Blah
settleDown x = x

type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 :: Object -> Sentence
s1 = Sentence "dogs" "drool"

s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs"

data Rocks = Rocks String deriving (Eq, Show, Ord)

data Yeah = Yeah Bool deriving (Eq, Show, Ord)

data Papu = Papu Rocks Yeah deriving (Eq, Show, Ord)

phew :: Papu
phew = Papu (Rocks "chases") (Yeah True)

truth :: Papu
truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

-- Match the types
-------------------------

f2 :: Float
f2 = 1.0

f3 :: Fractional a => a
f3 = 1.0

f4 :: RealFrac a => a
f4 = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

myX :: Int
myX = 1

sigmund :: Int -> Int
sigmund _ = myX

sigmund' :: Int -> Int
sigmund' _ = myX

jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

-- Type-Kwon-Do Two: Electric Typealoo
-------------------------

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = aToB a == b

arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith aToB n a = fromInteger n + aToB a
