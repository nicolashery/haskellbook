{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chapter11 where

import Data.Char (toUpper)

-- Vehicles

data Price = Price Integer
  deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata
  deriving (Eq, Show)

data Size = Size Integer
  deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu
getManu _ = error "No Manufacturer"

-- Logic Goats

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int
  deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

instance TooMany (Int, Int) where
  tooMany (n, m) = n + m > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, m) = tooMany (n + m)

-- How Does Your Garden Grow?

type Gardener = String

data Garden
  = Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving Show

-- Programmers

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer
  { os :: OperatingSystem
  , lang :: ProgLang
  } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [ Haskell
  , Agda
  , Idris
  , PureScript
  ]

allProgrammers :: [Programmer]
allProgrammers =
  [ Programmer
      { os = os'
      , lang = lang'
      }
  | os' <- allOperatingSystems
  , lang' <- allLanguages
  ]

-- Binary Tree

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node
    (Node Leaf 3 Leaf)
    1
    (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected =
  Node
  (Node Leaf 4 Leaf)
  2
  (Node Leaf 5 Leaf)

mapOkay :: IO ()
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) =
  [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) =
  inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) =
  postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree =
  Node
    (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "Bad news bears."

testAll :: IO ()
testAll = do
  testPreorder
  testInorder
  testPostorder

foldTree :: (a -> b -> b)
         -> b
         -> BinaryTree a
         -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) =
  foldTree f (foldTree f (f a b) left) right

-- As-patterns

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xs@(x:xs') (y:ys')
  | x == y = isSubseqOf xs' ys'
  | otherwise = isSubseqOf xs ys'

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map tupleWord . words
  where tupleWord [] = ([], [])
        tupleWord xs@(x:xs') = (toUpper x : xs', xs)

-- Language exercises

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph = undefined -- TODO

-- Phone exercise

data DaPhone = DaPhone -- TODO

-- Hutton's Razor

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add left right) = eval left + eval right

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add left right) =
  printExpr left ++ " + " ++ printExpr right
