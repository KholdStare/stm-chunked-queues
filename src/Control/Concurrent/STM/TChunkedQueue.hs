{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Control.Concurrent.STM.TChunkedQueue (

    -- * Simple Chunked Queue
    ChunkedQueue,
    consumeQueue,
    TChunkedQueue,

    -- * STM operations
    newTChunkedQueue,
    newTChunkedQueueIO,
    drainTChunkedQueue,
    tryDrainTChunkedQueue,
    writeTChunkedQueue,
    writeManyTChunkedQueue,
    isEmptyTChunkedQueue,

    -- * IO operations
    drainAndSettleTChunkedQueue,
    drainWithTimeoutTChunkedQueue,

) where

import Prelude             hiding (reads)
import Control.Applicative ((<$>))
{-import Control.Monad    (forever, void)-}
import Control.Monad
import Control.Monad.STM   (STM, retry, atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async (race)
import Control.Concurrent (threadDelay)

import Data.Monoid

----------------------------------------------------------------

-- TODO: make closeable?
data TChunkedQueue a = TChunkedQueue
    {-# UNPACK #-} !(TVar (ChunkedQueue a))
    
-----------------
data ChunkedQueue a = ChunkedQueue [Chunk a]

instance Monoid (ChunkedQueue a) where
    mempty = ChunkedQueue []
    (ChunkedQueue []) `mappend` b = b
    (ChunkedQueue a) `mappend` (ChunkedQueue b) = ChunkedQueue (b ++ a)

consumeQueue :: ChunkedQueue a -> [a]
consumeQueue (ChunkedQueue chunks) = go [] chunks
    where
        go acc [] = acc
        go acc (Forward x : xs) = go (x ++ acc) xs
        go acc (Reverse x : xs) = go ((reverse x) ++ acc) xs

enqueueOne :: ChunkedQueue a -> a -> ChunkedQueue a
enqueueOne (ChunkedQueue (Reverse xs:chunks)) x =
    ChunkedQueue (Reverse (x:xs):chunks)
enqueueOne (ChunkedQueue chunks) x =
    ChunkedQueue (Reverse [x]:chunks)


enqueueMany :: ChunkedQueue a -> [a] -> ChunkedQueue a
enqueueMany chQueue [x] = enqueueOne chQueue x
enqueueMany (ChunkedQueue chunks) xs = ChunkedQueue (Forward xs:chunks)


data Chunk a = Forward [a] | Reverse [a]


-- | Build and returns a new instance of @TChunkedQueue@
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


-- | Write many values to a @TChunkedQueue@
writeManyTChunkedQueue :: TChunkedQueue a -> [a] -> STM ()
writeManyTChunkedQueue (TChunkedQueue tChQueue) xs = do
    chQueue <- readTVar tChQueue
    writeTVar tChQueue $ enqueueMany chQueue xs

-- | Write a value to a @TChunkedQueue@
writeTChunkedQueue :: TChunkedQueue a -> a -> STM ()
writeTChunkedQueue (TChunkedQueue tChQueue) x = do
    chQueue <- readTVar tChQueue
    writeTVar tChQueue $ enqueueOne chQueue x


isEmptyTChunkedQueue :: TChunkedQueue a -> STM Bool
isEmptyTChunkedQueue (TChunkedQueue tChQueue) = do
    ChunkedQueue chunks <- readTVar tChQueue
    return $ null chunks


-- Keep draining the queue until no more items are seen for at least
-- the given timeout period. Blocks if the queue is empty to begin with,
-- and starts timing after the first value appears in the queue.
drainAndSettleTChunkedQueue :: Int -> TChunkedQueue a -> IO (ChunkedQueue a)
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


-- Keep draining the queue for at least the specified time period. Blocks if
-- the queue is empty to begin with, and starts timing as soon as the first
-- value appears in the queue.
drainWithTimeoutTChunkedQueue :: Int -> TChunkedQueue a -> IO (ChunkedQueue a)
drainWithTimeoutTChunkedQueue delay queue = do
    stashedQueue <- newTChunkedQueueIO

    let transferItems = atomically $ do
        items <- drainTChunkedQueue queue
        stashedQueue `writeManyTChunkedQueue` (consumeQueue items)

    transferItems -- run tranfer once before timing, so we block on empty queue.

    withTimeout delay (forever transferItems)
    atomically $ drainTChunkedQueue stashedQueue

    where 
        withTimeout t action = void $ action `race` threadDelay t


----------------------------------------------------------------
----------------------------------------------------------- fin.
