module Chapter07 where

-- Variety Pack

k :: (a, b) -> a
k (x, _) = x

f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (a, _, c) (d, _, f') = ((a, d), (c, f'))

-- Case Practice

functionC :: (Ord a) => a -> a -> a
functionC x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 :: (Integral a) => a -> a
ifEvenAdd2 n =
  case even n of
    True -> n + 2
    False -> n

nums :: (Ord a, Num a) => a -> a
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- Artful Dodgy

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 20

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

-- Let's write code

tensDigit :: Integral a => a -> a
tensDigit x = d
    where (d, _) = x `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = d2
    where d = tensDigit x
          d2 = tensDigit d

foldBool :: a -> a -> Bool -> a
foldBool x y t =
  case t of
    False -> x
    True -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y t
    | t = y
    | otherwise = x


g :: (a -> b) -> (a, c) -> (b, c)
g aToB (a, c) = (aToB a, c)

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

testRoundTrip :: IO ()
testRoundTrip = do
  print (roundTrip (4 :: Integer) :: Integer)
  print (id (4 :: Integer) :: Integer)
