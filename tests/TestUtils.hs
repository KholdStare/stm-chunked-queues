{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RecordWildCards #-}
module TestUtils (

    MaybeDrainFunc,
    DrainFunc,
    WriteFunc,
    CloseFunc,
    TestableChunkedQueue(..),
    QueueAction(..),
    consumingThread,
    runQueueActions,
    testActions,

) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent (threadDelay)

import Control.Concurrent.STM.TChunkedQueue (ChunkedQueue, consumeQueue)

import Test.Tasty.HUnit

type MaybeDrainFunc q a =  (q a -> IO (Maybe (ChunkedQueue a)))
type DrainFunc q a =  (q a -> IO (ChunkedQueue a))
type WriteFunc q a =  (q a -> a -> STM ())
type CloseFunc q a =  (q a -> STM ())

data TestableChunkedQueue q a = TestableChunkedQueue {
    createQueue :: IO (q a),
    maybeDrain  :: MaybeDrainFunc q a,
    drain       :: DrainFunc q a,
    write       :: WriteFunc q a,
    close       :: CloseFunc q a
}

consumingThread :: MaybeDrainFunc q a
                -> DrainFunc q a
                -> q a
                -> TVar Bool
                -> IO [Maybe [a]]
consumingThread maybeDrainFunc drainFunc queue areWeDone = go []
    where
        go acc = do
            chQueue <- maybeDrainFunc queue
            let allItems = (consumeQueue <$> chQueue) : acc

            done <- atomically $ readTVar areWeDone

            if done
            then do
                remainingItems <- consumeQueue <$> drainFunc queue
                let result = reverse $
                        if null remainingItems 
                            then allItems
                            else (Just remainingItems) : allItems
                    
                return result

            else go allItems


data QueueAction a = Enqueue [a] | Wait Int | Close


runQueueActions :: TestableChunkedQueue q a
                -> [QueueAction a]
                -> IO [Maybe [a]]
runQueueActions TestableChunkedQueue{..} actions = do
    queue <- createQueue
    areWeDone <- atomically $ newTVar False

    asyncResults <- async $
        consumingThread maybeDrain drain queue areWeDone

    let dumpToQueue = mapM_ (atomically . write queue) 

    forM_ actions $ \action ->
        case action of
            Enqueue items -> dumpToQueue items
            Wait delay    -> threadDelay delay
            Close         -> atomically $ close queue

    atomically $ writeTVar areWeDone True

    wait asyncResults


testActions :: (Eq a, Show a)
            => TestableChunkedQueue q a
            -> [QueueAction a]
            -> [Maybe [a]]
            -> Assertion
testActions queue actions expectations = do
    results <- runQueueActions queue actions
    assertEqual "items dequeued have to be grouped correctly" expectations results
