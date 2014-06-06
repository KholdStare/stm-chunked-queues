{-# LANGUAGE DoAndIfThenElse #-}
module TMChunkedQueue (
    tests
) where

{-import Control.Applicative ((<$>))-}
import Control.Monad.STM   (atomically)
import Data.Monoid

import Control.Concurrent.STM.TMChunkedQueue

import TestUtils

import Test.Tasty
import Test.Tasty.HUnit

testActions :: (Eq a, Show a)
            => [QueueAction a]
            -> [Maybe [a]]
            -> Assertion
testActions actions expectations = do
    queue <- newTMChunkedQueueIO
    results <- runQueueActions (drainAndSettleTMChunkedQueue 20000)
                              ((fmap collapseMaybe) . atomically . tryDrainTMChunkedQueue)
                              writeTMChunkedQueue
                              queue
                              actions
    assertEqual "items dequeued have to be grouped correctly" expectations results

    where
        collapseMaybe Nothing = mempty
        collapseMaybe (Just xs) = xs

-- | Convenience specialization to avoid "Default Constraint" warnings
testActions' :: [QueueAction Int] -> [Maybe [Int]] -> Assertion
testActions' = testActions


tests :: [TestTree]
tests = 
    let shortPause = 10000
        longPause  = 60000
    in

    [ testCase "Simple" $ testActions' [Enqueue [0]] $ expect [Just [0]]
    , testCase "Two" $ testActions' [
            Enqueue [0],
            Wait shortPause,
            Enqueue [1]]
            $ expect [Just [0, 1]]
    , testCase "Two With Pause" $ testActions' [
            Enqueue [0],
            Wait longPause,
            Enqueue [1]]
            $ expect [Just [0], Just [1]]
    , testCase "Two Lists" $ testActions' [
            Enqueue [0..10],
            Wait shortPause,
            Enqueue [11..20]]
            $ expect [Just [0..20]]
    , testCase "Many With Pauses" $ testActions' [
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
