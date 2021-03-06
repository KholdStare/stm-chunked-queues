{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE DeriveDataTypeable #-}

----------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TChunkedQueue
-- Copyright   :  Copyright (c) 2014 Alexander Kondratskiy
-- License     :  BSD3
-- Maintainer  :  Alexander Kondratskiy <kholdstare0.0@gmail.com>
-- Portability :  non-portable (GHC STM, DeriveDataTypeable)
--
-- A version of "Control.Concurrent.STM.TQueue" that allows complete draining.
-- This makes it possible to chunk items based on a timeout or a settle
-- period. This is useful when items/requests arriving through the queue are
-- too granular and have to be combined, while retaining responsiveness.
--
-- Some capabilities of @TQueue@ are missing (such as unget) due to design
-- tradeoffs.
--
-- /Since: 0.1.0/
------------------------------------------------------------------
module Control.Concurrent.STM.TChunkedQueue (

    -- * The TChunkedQueue type
    TChunkedQueue,
    ChunkedQueue,
    consumeQueue,

    -- ** Creating TChunkedQueues
    newTChunkedQueue,
    newTChunkedQueueIO,
    -- ** Draining TChunkedQueues
    drainTChunkedQueue,
    tryDrainTChunkedQueue,
    -- ** Writing to TChunkedQueues
    writeTChunkedQueue,
    writeManyTChunkedQueue,
    -- ** Predicates
    isEmptyTChunkedQueue,

    -- * Chunked operations
    drainAndSettleTChunkedQueue,
    drainWithTimeoutTChunkedQueue,

) where

import Data.Typeable       (Typeable)
import Prelude             hiding (reads)
import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.STM   (STM, retry, atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async (race)
import Control.Concurrent (threadDelay)

import Control.Concurrent.STM.ChunkedQueue

----------------------------------------------------------------

-- | @TChunkedQueue@ is an abstract type representing a drainable FIFO queue.
data TChunkedQueue a = TChunkedQueue
    {-# UNPACK #-} !(TVar (ChunkedQueue a))
    deriving Typeable


-- | Build and return a new instance of @TChunkedQueue@
newTChunkedQueue :: STM (TChunkedQueue a)
newTChunkedQueue = TChunkedQueue <$> newTVar (ChunkedQueue [])


-- | @IO@ version of 'newTChunkedQueue'
newTChunkedQueueIO :: IO (TChunkedQueue a)
newTChunkedQueueIO = TChunkedQueue <$> newTVarIO (ChunkedQueue [])


-- | Drain everything contained in the @TChunkedQueue@, but block if it is
-- empty. Corollary: never returns empty queue.
drainTChunkedQueue :: TChunkedQueue a -> STM (ChunkedQueue a)
drainTChunkedQueue (TChunkedQueue tChQueue) = do
    chQueue <- readTVar tChQueue
    case chQueue of
        ChunkedQueue [] -> retry
        _ -> do
            writeTVar tChQueue (ChunkedQueue [])
            return chQueue


-- | Drain everything contained in the @TChunkedQueue@. Doesn't block
tryDrainTChunkedQueue :: TChunkedQueue a -> STM (ChunkedQueue a)
tryDrainTChunkedQueue (TChunkedQueue tChQueue) = do
    chQueue <- readTVar tChQueue
    case chQueue of
        ChunkedQueue [] -> return ()
        _ -> writeTVar tChQueue (ChunkedQueue [])

    return chQueue


-- | Write many values to a @TChunkedQueue@. More efficient than
-- @writeTChunkedQueue@, so prefer using this variant if several items have to
-- be enqueued
writeManyTChunkedQueue :: TChunkedQueue a -> [a] -> STM ()
writeManyTChunkedQueue (TChunkedQueue tChQueue) xs = do
    chQueue <- readTVar tChQueue
    writeTVar tChQueue $ enqueueMany chQueue xs


-- | Write a value to a @TChunkedQueue@
writeTChunkedQueue :: TChunkedQueue a -> a -> STM ()
writeTChunkedQueue (TChunkedQueue tChQueue) x = do
    chQueue <- readTVar tChQueue
    writeTVar tChQueue $ enqueueOne chQueue x


-- | Return @True@ if the supplied @TChunkedQueue@ is empty.
isEmptyTChunkedQueue :: TChunkedQueue a -> STM Bool
isEmptyTChunkedQueue (TChunkedQueue tChQueue) = do
    ChunkedQueue chunks <- readTVar tChQueue
    return $ null chunks


----------------------------------------------------------------


-- | Keep draining the queue until no more items are seen for at least
-- the given timeout period. Blocks if the queue is empty to begin with,
-- and starts timing after the first value appears in the queue.
drainAndSettleTChunkedQueue :: Int -- ^ settle period in microseconds
                             -> TChunkedQueue a
                             -> IO (ChunkedQueue a)
drainAndSettleTChunkedQueue delay queue = do
    ChunkedQueue chunks <- atomically $ drainTChunkedQueue queue
    -- chunks by definition is non-empty here
    go chunks

    where
        go acc = do
            threadDelay delay
            ChunkedQueue chunks <- atomically $ tryDrainTChunkedQueue queue
            case chunks of
                [] -> return $ ChunkedQueue acc
                _ -> go (chunks ++ acc)


-- | Keep draining the queue for at least the specified time period. Blocks if
-- the queue is empty to begin with, and starts timing as soon as the first
-- value appears in the queue.
drainWithTimeoutTChunkedQueue :: Int -- ^ timeout in microseconds
                               -> TChunkedQueue a
                               -> IO (ChunkedQueue a)
drainWithTimeoutTChunkedQueue delay queue = do
    stashedQueue <- newTChunkedQueueIO

    let transferItems = atomically $ do
        items <- drainTChunkedQueue queue
        stashedQueue `writeManyTChunkedQueue` (consumeQueue items)

    transferItems -- run transfer once before timing, which blocks on empty queue.

    withTimeout delay (forever transferItems)
    atomically $ drainTChunkedQueue stashedQueue

    where 
        withTimeout t action = void $ action `race` threadDelay t


----------------------------------------------------------------
