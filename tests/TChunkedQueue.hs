{-# LANGUAGE DoAndIfThenElse #-}
module TChunkedQueue (
    tests
) where

{-import Control.Applicative ((<$>))-}
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
                remainingItems <- atomically $ tryDrainTChunkedQueue queue
                return $ reverse (consumeQueue remainingItems : allItems)
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

    forM_ actions $ \action -> do
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
    assertEqual "items dequeued have to be correct" results expectations

tests :: [TestTree]
tests = [ testCase "Simple" (testActions [Enqueue [0]] [[0],[]]) ]
{-tests = [ testProperty "No refraction if index unchanged" prop_sameIndex-}
        {-, testProperty "Critical angle in glass" $ forAll (choose (0, glassCriticalAngle))-}
                        {-$ checkTotalInternalReflection glassRefractiveIndex ]-}
