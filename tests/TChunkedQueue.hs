{-# LANGUAGE DoAndIfThenElse #-}
module TChunkedQueue (
    tests
) where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.STM   (atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async (wait, async)
import Control.Concurrent

import Control.Concurrent.STM.TChunkedQueue

import Test.Tasty
import Test.Tasty.HUnit

type DrainFunc a =  (TChunkedQueue a -> IO (ChunkedQueue a))

consumingThread :: DrainFunc a
                -> TVar Bool
                -> TChunkedQueue a
                -> IO [[a]]
consumingThread pullFunc areWeDone queue = go []
    where
        go acc = do
            chQueue <- pullFunc queue
            let allItems = (consumeQueue chQueue) : acc

            done <- atomically $ readTVar areWeDone

            if done
            then do
                remainingItems <- consumeQueue <$> atomically (tryDrainTChunkedQueue queue)
                let result = reverse $
                        if null remainingItems 
                            then allItems
                            else remainingItems : allItems
                    
                return result

            else go allItems


data QueueAction a = Enqueue [a] | Wait Int


runQueueActions :: [QueueAction a]
                -> IO [[a]]
runQueueActions actions = do
    queue <- newTChunkedQueueIO
    areWeDone <- atomically $ newTVar False

    -- TODO abstract over DrainFunc
    let settleDelay = 20000
    asyncResults <- async $
        consumingThread (drainAndSettleTChunkedQueue settleDelay) areWeDone queue

    let dumpToQueue = 
            mapM_ (atomically . writeTChunkedQueue queue) 

    forM_ actions $ \action ->
        case action of
            Enqueue items -> dumpToQueue items
            Wait delay -> threadDelay delay

    atomically $ writeTVar areWeDone True

    wait asyncResults


testActions :: (Eq a, Show a)
            => [QueueAction a]
            -> [[a]]
            -> Assertion
testActions actions expectations = do
    results <- runQueueActions actions
    assertEqual "items dequeued have to be grouped correctly" expectations results

-- | Convenience specialization to avoid "Default Constraint" warnings
testActions' :: [QueueAction Int] -> [[Int]] -> Assertion
testActions' = testActions


tests :: [TestTree]
tests = 
    let shortPause = 10000
        longPause  = 60000
    in

    [ testCase "Simple" $ testActions' [Enqueue [0]] $ expect [[0]]
    , testCase "Two" $ testActions' [
            Enqueue [0],
            Wait shortPause,
            Enqueue [1]]
            $ expect [[0, 1]]
    , testCase "Two With Pause" $ testActions' [
            Enqueue [0],
            Wait longPause,
            Enqueue [1]]
            $ expect [[0], [1]]
    , testCase "Two Lists" $ testActions' [
            Enqueue [0..10],
            Wait shortPause,
            Enqueue [11..20]]
            $ expect [[0..20]]
    , testCase "Many With Pauses" $ testActions' [
            Enqueue [0..10], Wait shortPause, Enqueue [11..20],
            Wait longPause,
            Enqueue [21..30],
            Wait longPause,
            Enqueue [31..40], Wait shortPause, Enqueue [41..50],
            Wait longPause,
            Enqueue [51..60] ]
            $ expect [[0..20], [21..30], [31..50], [51..60]]
    ]

    where
        expect = id
