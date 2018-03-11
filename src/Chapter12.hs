module Chapter12 where

-- String processing

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe w = Just w

replaceThe :: String -> String
replaceThe = unwords . map (replace . notThe) . words
  where replace Nothing = "a"
        replace (Just w) = w

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go 0 (map notThe $ words s)
  where go count [] = count
        go count [_] = count
        go count [Nothing, Just w] = incrementIfVowel count w
        go count (Nothing:Just w:ws) = go (incrementIfVowel count w) ws
        go count (_:_:ws) = go count ws
        incrementIfVowel count [] = count
        incrementIfVowel count (c:_) =
          if c `elem` "aeiou" then count + 1 else count

countVowels :: String -> Int
countVowels = length . filter isVowel
  where isVowel c = c `elem` "aeiou"

-- Validate the word

newtype Word' = Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s
  | vowelCount > consonantCount = Nothing
  | otherwise = Just $ Word' s
  where vowelCount = countVowels s
        consonantCount = length s - vowelCount

-- It's only Natural

data Nat = Zero | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | otherwise = Just $ convert n
  where convert 0 = Zero
        convert m = Succ $ convert (m - 1)

-- Small library for Maybe

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z _ Nothing = z
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe z Nothing = z
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = concat . map maybeToList

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f (Just [])
  where f Nothing _ = Nothing
        f _ Nothing = Nothing
        f (Just a) (Just b) = Just (a : b)

-- Small library for Either

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where f (Right _) xs = xs
        f (Left x) xs = x : xs

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f (Left _) ys = ys
        f (Right y) ys = y : ys

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

-- Unfolds

myIterate :: (a -> a) -> a -> [a]
myIterate next x = x : myIterate next (next x)

myUnfoldr :: (b -> Maybe (a, b))
          -> b
          -> [a]
myUnfoldr step x = case step x of
  Nothing -> []
  Just (a, b) -> a : myUnfoldr step b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (a, f a))

-- Finally something other than a list!

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a))
       -> a
       -> BinaryTree b
unfold step x = case step x of
  Nothing -> Leaf
  Just (a, b, a') -> Node (unfold step a) b (unfold step a')

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold step 0
  where step x
          | x == n = Nothing
          | otherwise = Just (x + 1, x, x + 1)
