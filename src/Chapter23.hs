{-# LANGUAGE InstanceSigs #-}

module Chapter23 where

import Control.Monad.Trans.State (State, execState, get, put)
import System.Random (StdGen, randomR)

-- Roll Your Own

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go total count gen
      | total >= limit = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (total + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged limit g = go 0 [] g
  where
    go :: Int -> [Die] -> StdGen -> (Int, [Die])
    go total rolls gen
      | total >= limit = (length rolls, rolls)
      | otherwise =
        let (n, nextGen) = randomR (1, 6) gen
            die = intToDie n
        in go (total + n) (rolls ++ [die]) nextGen

-- Write State for yourself

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi g'
    where g' s = let (a, s') = g s
                 in (f a, s')

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi g'
    where g' s = let (aToB, s') = f s
                     (a, s'') = g s'
                  in (aToB a, s'')

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi f'
    where f' s = let (a, s') = f s
                     (Moi h) = g a
                 in h s'

-- FizzBuzz

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to =
  execState (mapM_ addResult list) []
  where
    list = enumFromThenTo to (pred to) from

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

main :: IO ()
main =
  mapM_ putStrLn $ fizzbuzzFromTo 1 100

-- Chapter exercises

get' :: Moi s s
get' = Moi $ \s -> (s, s)

put' :: s -> Moi s ()
put' s = Moi $ const ((), s)

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s

eval :: Moi s a -> s -> a
eval (Moi sa) = fst . sa

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)
