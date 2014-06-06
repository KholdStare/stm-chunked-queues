{-# LANGUAGE DoAndIfThenElse #-}
module TestUtils (

    MaybeDrainFunc,
    DrainFunc,
    WriteFunc,
    QueueAction(..),
    consumingThread,
    runQueueActions,

) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent (threadDelay)

import Control.Concurrent.STM.TChunkedQueue (ChunkedQueue, consumeQueue)

type MaybeDrainFunc q a =  (q a -> IO (Maybe (ChunkedQueue a)))
type DrainFunc q a =  (q a -> IO (ChunkedQueue a))
type WriteFunc q a =  (q a -> a -> STM ())

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


data QueueAction a = Enqueue [a] | Wait Int


runQueueActions :: MaybeDrainFunc q a
                -> DrainFunc q a
                -> WriteFunc q a
                -> q a
                -> [QueueAction a]
                -> IO [Maybe [a]]
runQueueActions maybeDrainFunc drainFunc writeFunc queue actions = do
    areWeDone <- atomically $ newTVar False

    asyncResults <- async $
        consumingThread maybeDrainFunc drainFunc queue areWeDone

    let dumpToQueue = mapM_ (atomically . writeFunc queue) 

    forM_ actions $ \action ->
        case action of
            Enqueue items -> dumpToQueue items
            Wait delay -> threadDelay delay

    atomically $ writeTVar areWeDone True

    wait asyncResults
