{-# LANGUAGE DoAndIfThenElse #-}
module TChunkedQueue (
    tests,
    testWith,
) where

import Control.Applicative ((<$>))
import Control.Monad.STM   (atomically)

import Control.Concurrent.STM.TChunkedQueue

import TestUtils

import Test.Tasty
import Test.Tasty.HUnit

-- | Convenience specialization to avoid "Default Constraint" warnings
testActions' :: [QueueAction Int] -> [Maybe [Int]] -> Assertion
testActions' = testActions $
        TestableChunkedQueue
            newTChunkedQueueIO
            (\q -> Just <$> drainAndSettleTChunkedQueue 20000 q)
            (atomically . tryDrainTChunkedQueue)
            writeTChunkedQueue
            (const $ return ()) -- no close function


testWith :: ([QueueAction Int] -> [Maybe [Int]] -> Assertion) -> [TestTree]
testWith assertActions = 

    let shortPause = 10000
        longPause  = 60000
    in

    [ testCase "Simple" $ assertActions [Enqueue [0]] $ expect [Just [0]]
    , testCase "Two" $ assertActions [
            Enqueue [0],
            Wait shortPause,
            Enqueue [1]]
            $ expect [Just [0, 1]]
    , testCase "Two With Pause" $ assertActions [
            Enqueue [0],
            Wait longPause,
            Enqueue [1]]
            $ expect [Just [0], Just [1]]
    , testCase "Two Lists" $ assertActions [
            Enqueue [0..10],
            Wait shortPause,
            Enqueue [11..20]]
            $ expect [Just [0..20]]
    , testCase "Many With Pauses" $ assertActions [
            Enqueue [0..10], Wait shortPause, Enqueue [11..20],
            Wait longPause,
            Enqueue [21..30],
            Wait longPause,
            Enqueue [31..40], Wait shortPause, Enqueue [41..50],
            Wait longPause,
            Enqueue [51..60] ]
            $ expect [Just [0..20], Just [21..30], Just [31..50], Just [51..60]]
    ]

    where
        expect = id


tests :: [TestTree]
tests = testWith testActions'
