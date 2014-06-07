{-# LANGUAGE DoAndIfThenElse #-}
module TMChunkedQueue (
    tests
) where

import Control.Monad.STM   (atomically)
import Data.Monoid

import Control.Concurrent.STM.TMChunkedQueue

import TestUtils
import qualified TChunkedQueue

import Test.Tasty
import Test.Tasty.HUnit


-- | Convenience specialization to avoid "Default Constraint" warnings
settleTester :: [QueueAction Int] -> [Maybe [Int]] -> Assertion
settleTester = testActions $
        TestableChunkedQueue
            newTMChunkedQueueIO
            (drainAndSettleTMChunkedQueue 20000)
            ((fmap collapseMaybe) . atomically . tryDrainTMChunkedQueue)
            writeTMChunkedQueue
            closeTMChunkedQueue

    where
        collapseMaybe Nothing = mempty
        collapseMaybe (Just xs) = xs


timeoutTester :: Int -> [QueueAction Int] -> [Maybe [Int]] -> Assertion
timeoutTester timeout = testActions $
        TestableChunkedQueue
            newTMChunkedQueueIO
            (drainWithTimeoutTMChunkedQueue timeout)
            ((fmap collapseMaybe) . atomically . tryDrainTMChunkedQueue)
            writeTMChunkedQueue
            closeTMChunkedQueue

    where
        collapseMaybe Nothing = mempty
        collapseMaybe (Just xs) = xs

shortPause, longPause :: Int
shortPause = 10000
longPause  = 60000

testClosing :: ([QueueAction Int] -> [Maybe [Int]] -> Assertion) -> [TestTree]
testClosing assertActions =
    

    [ testCase "Two" $ assertActions [
            Enqueue [0],
            Close,
            Enqueue [1]]
            $ expect [Just [0]]

    , testCase "Two with wait" $ assertActions [
            Enqueue [0],
            Wait shortPause,
            Close,
            Enqueue [1]]
            $ expect [Just [0]]
    ]

    where
        expect = id


tests :: [TestTree]
tests = 
    [ testGroup "Settle"

        [ testGroup "Non-Closing" $ TChunkedQueue.testWith settleTester
        , testGroup "Closing" $ testClosing settleTester ]

    , testGroup "Timeout"

        [ testGroup "Non-Closing"
            [ testCase "One" $ timeoutTester shortPause [
                    Enqueue [0],
                    Wait longPause,
                    Enqueue [1]]
                    $ expect [Just [0], Just [1]]

            , testCase "Two" $ timeoutTester longPause [
                    Enqueue [0],
                    Wait shortPause,
                    Enqueue [1],
                    Wait longPause,
                    Enqueue [2]]
                    $ expect [Just [0, 1], Just [2]]

            , testCase "Many" $ timeoutTester longPause (
                    concatMap (\i -> [Enqueue [i], Wait shortPause]) [1..10]
                    )
                    $ expect [Just [1..6], Just [7..10]]
            ]

        , testGroup "Closing" $ testClosing $ timeoutTester shortPause
        ]
    ]

    where
        expect = id
