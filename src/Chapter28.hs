module Chapter28 where

import Data.List (uncons)
import qualified Data.Map as M
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

-- Benchmark Practice

bumpIt :: (Int, Int) -> (Int, Int)
bumpIt (i, v) = (i + 1, v + 1)

bigMap :: M.Map Int Int
bigMap = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0, 0)

bigSet :: S.Set Int
bigSet = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

insertMap :: Int -> M.Map Int Int
insertMap k = M.insert k 1 bigMap

insertSet :: Int -> S.Set Int
insertSet v = S.insert v bigSet

unionMap :: Int -> M.Map Int Int
unionMap n = M.union m bigMap
  where m = M.fromList $ take 100 stream
        stream = iterate bumpIt (n, n)

unionSet :: Int -> S.Set Int
unionSet n = S.union s bigSet
  where s = S.fromList $ take 100 stream
        stream = iterate (+1) n

-- Vector

bigVector :: V.Vector Int
bigVector = V.fromList [1..10000]

bigUnboxedVector :: UV.Vector Int
bigUnboxedVector = UV.fromList [1..10000]

sliceVector :: Int -> V.Vector Int
sliceVector n = V.slice 0 n bigVector

sliceUnboxedVector :: Int -> UV.Vector Int
sliceUnboxedVector n = UV.slice 0 n bigUnboxedVector

-- Difference List

newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL $ const []
{-# INLINE empty #-}

singleton :: a -> DList a
singleton x = DL $ const [x]
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList xs = unDL xs []
{-# INLINE toList #-}

infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL $ (x:) . unDL xs
{-# INLINE cons #-}

infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL $ (++ [x]) . unDL xs
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append xs ys = DL (\zs -> unDL xs zs ++ unDL ys zs)
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (singleton n `append` xs)

-- A simple queue

-- See "Purely functional data structures" (section `3.1.1`):
-- https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf
data Queue a =
  Queue {
      -- End of the queue, newest first
      enqueue :: [a]
      -- Front of the queue, oldest first
    , dequeue :: [a]
    }
  deriving (Eq, Show)

-- Smart constructor, enforcing the invariant:
-- `dequeue` is only ever empty if `enqueue` is also empty,
-- i.e. if the whole queue is empty
mkQueue :: [a] -> [a] -> Queue a
mkQueue [] [] = Queue [] []
mkQueue enq [] = Queue [] (reverse enq)
mkQueue enq deq = Queue enq deq

emptyQueue :: Queue a
emptyQueue = mkQueue [] []

pushQueue :: a -> Queue a -> Queue a
pushQueue x Queue{enqueue = enq, dequeue = deq} =
  mkQueue (x : enq) deq

popQueue :: Queue a -> Maybe (a, Queue a)
popQueue Queue{enqueue = enq, dequeue = deq} =
  case deq of
    -- The invariant of our datastructure enforces
    -- that `dequeue` is only every empty if the whole
    -- queue is empty
    [] -> Nothing
    (x:deq') -> Just (x, mkQueue enq deq')

queueFromList :: [a] -> Queue a
queueFromList xs = mkQueue [] xs

pushList :: a -> [a] -> [a]
pushList x xs = xs ++ [x]

popList :: [a] -> Maybe (a, [a])
popList = uncons

pushSequence :: a -> Seq a -> Seq a
pushSequence x xs = xs |> x

popSequence :: Seq a -> Maybe (a, Seq a)
popSequence xs = do
  x <- SQ.lookup 0 xs
  return (x, SQ.drop 1 xs)

bigQueue :: Queue Int
bigQueue = queueFromList [1..10000]

bigList :: [Int]
bigList = [1..10000]

bigSequence :: Seq Int
bigSequence = SQ.fromList [1..10000]

pushPopQueue :: Int -> Maybe (Int, Queue Int)
pushPopQueue x = popQueue $ pushQueue x bigQueue

pushPopList :: Int -> Maybe (Int, [Int])
pushPopList x = popList $ pushList x bigList

pushPopSequence :: Int -> Maybe (Int, Seq Int)
pushPopSequence x = popSequence $ pushSequence x bigSequence
