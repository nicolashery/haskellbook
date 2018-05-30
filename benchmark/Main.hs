module Main where

import Criterion.Main (bench, bgroup, defaultMain, whnf)

import Chapter28
    ( constructDlist
    , insertMap
    , insertSet
    , pushPopList
    , pushPopQueue
    , pushPopSequence
    , schlemiel
    , sliceUnboxedVector
    , sliceVector
    , unionMap
    , unionSet
    )

main :: IO ()
main = defaultMain [
    bgroup "map vs set" [
      bench "insert map" $
      whnf insertMap 9999
    , bench "insert set" $
      whnf insertSet 9999
    , bench "union map" $
      whnf unionMap 500
    , bench "union set" $
      whnf unionSet 500
    , bench "slice set" $
      whnf unionSet 500
    ]

  , bgroup "vector" [
      bench "slice vector" $
      whnf sliceVector 1000
    , bench "slice unboxed vector" $
      whnf sliceUnboxedVector 1000
    ]

  , bgroup "dlist" [
      bench "concat list" $
      whnf schlemiel 123456
    , bench "concat dlist" $
      whnf constructDlist 123456
    ]

  , bgroup "queue" [
      bench "push pop queue" $
      whnf pushPopQueue 10001
    , bench "push pop list" $
      whnf pushPopList 10001
    , bench "push pop sequence" $
      whnf pushPopSequence 10001
    ]
  ]
