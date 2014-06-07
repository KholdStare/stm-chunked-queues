{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE DeriveDataTypeable #-}

----------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TMChunkedQueue
-- Copyright   :  Copyright (c) 2014 Alexander Kondratskiy
-- License     :  BSD3
-- Maintainer  :  Alexander Kondratskiy <kholdstare0.0@gmail.com>
-- Portability :  non-portable (GHC STM, DeriveDataTypeable)
--
-- A version of @Control.Concurrent.STM.TQueue@ where the queue is closeable
-- and allows complete draining. This makes it possible to chunk items based on
-- a timeout or a "settle period". This is useful when items/requests arriving
-- through the queue are too granular and have to be combined, while retaining
-- responsiveness.
--
-- Some capabilities of @TQueue@ are missing (such as unget) due to design
-- tradeoffs.
--
-- /Since: 0.1.0/
------------------------------------------------------------------
module Control.Concurrent.STM.TMChunkedQueue (

    -- * The TMChunkedQueue type
    TMChunkedQueue,
    ChunkedQueue,
    consumeQueue,

    -- ** Creating TMChunkedQueues
    newTMChunkedQueue,
    newTMChunkedQueueIO,
    -- ** Draining TMChunkedQueues
    drainTMChunkedQueue,
    tryDrainTMChunkedQueue,
    -- ** Writing to TMChunkedQueues
    writeTMChunkedQueue,
    writeManyTMChunkedQueue,
    -- ** Closing TMChunkedQueues
    closeTMChunkedQueue,
    -- ** Predicates
    isEmptyTMChunkedQueue,
    isClosedTMChunkedQueue,

    -- * Chunked operations
    drainAndSettleTMChunkedQueue,
    drainWithTimeoutTMChunkedQueue,

) where

import Data.Typeable       (Typeable)
import Prelude             hiding (reads)
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.STM   (STM, atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async (race)
import Control.Concurrent (threadDelay)

import Control.Concurrent.STM.ChunkedQueue
import Control.Concurrent.STM.TChunkedQueue

----------------------------------------------------------------

-- | @TMChunkedQueue@ is an abstract type representing a closeable, drainable
-- FIFO queue.
data TMChunkedQueue a = TMChunkedQueue {
    _isClosed :: {-# UNPACK #-} !(TVar Bool),
    _queue :: {-# UNPACK #-} !(TChunkedQueue a)
} deriving Typeable


-- | Build and returns a new instance of @TMChunkedQueue@
newTMChunkedQueue :: STM (TMChunkedQueue a)
newTMChunkedQueue =
    TMChunkedQueue <$>
    newTVar False <*>
    newTChunkedQueue


-- | @IO@ version of 'newTMChunkedQueue'
newTMChunkedQueueIO :: IO (TMChunkedQueue a)
newTMChunkedQueueIO =
    TMChunkedQueue <$>
    newTVarIO False <*>
    newTChunkedQueueIO



nonEmptyList :: [a] -> Maybe [a]
nonEmptyList [] = Nothing
nonEmptyList xs = Just xs

nonEmptyChunkedQueue :: ChunkedQueue a -> Maybe (ChunkedQueue a)
nonEmptyChunkedQueue (ChunkedQueue xs) = ChunkedQueue <$> nonEmptyList xs


-- | Drain everything contained in the @TMChunkedQueue@, but block if it is
-- empty. Corollary: never returns empty queue.
--
-- * Closed, Empty     - @Nothing@
-- * Closed, Non-Empty - @Just [...]@
-- * Open,   Empty     - @Blocks@
-- * Open,   Non-Empty - @Just [...]@
--
drainTMChunkedQueue :: TMChunkedQueue a -> STM (Maybe (ChunkedQueue a))
drainTMChunkedQueue (TMChunkedQueue closed queue) = do
    isClosed <- readTVar closed
    if isClosed
    then nonEmptyChunkedQueue <$> tryDrainTChunkedQueue queue
    else Just <$> drainTChunkedQueue queue


-- | Drain everything contained in the @TMChunkedQueue@. Doesn't block.
--
-- * Closed, Empty     - @Nothing@
-- * Closed, Non-Empty - @Just [...]@
-- * Open,   Empty     - @Just []@
-- * Open,   Non-Empty - @Just [...]@
--
tryDrainTMChunkedQueue :: TMChunkedQueue a -> STM (Maybe (ChunkedQueue a))
tryDrainTMChunkedQueue (TMChunkedQueue closed queue) = do
    isClosed <- readTVar closed
    if isClosed
    then nonEmptyChunkedQueue <$> tryDrainTChunkedQueue queue
    else Just <$> tryDrainTChunkedQueue queue


doIfOpen :: TMChunkedQueue a -> (TChunkedQueue a -> STM ()) -> STM ()
doIfOpen (TMChunkedQueue closed queue) action = do
    isClosed <- readTVar closed
    unless isClosed $ action queue


-- | Write many values to a @TMChunkedQueue@
writeManyTMChunkedQueue :: TMChunkedQueue a -> [a] -> STM ()
writeManyTMChunkedQueue queue xs =
    void $ doIfOpen queue $ flip writeManyTChunkedQueue xs


-- | Write a value to a @TMChunkedQueue@
writeTMChunkedQueue :: TMChunkedQueue a -> a -> STM ()
writeTMChunkedQueue queue x =
    void $ doIfOpen queue $ flip writeTChunkedQueue x


-- | Returns @True@ if the supplied @TMChunkedQueue@ is empty.
isEmptyTMChunkedQueue :: TMChunkedQueue a -> STM Bool
isEmptyTMChunkedQueue (TMChunkedQueue _ queue) = isEmptyTChunkedQueue queue


-- | Closes the @TMQueue@, preventing any further writes.
closeTMChunkedQueue :: TMChunkedQueue a -> STM ()
closeTMChunkedQueue (TMChunkedQueue closed _queue) =
    writeTVar closed True


-- | Returns @True@ if the supplied @TMChunkedQueue@ has been closed.
isClosedTMChunkedQueue :: TMChunkedQueue a -> STM Bool
isClosedTMChunkedQueue (TMChunkedQueue closed _queue) =
    readTVar closed


----------------------------------------------------------------


-- | Keep draining the queue until no more items are seen for at least
-- the given timeout period. Blocks if the queue is empty to begin with,
-- and starts timing after the first value appears in the queue.
drainAndSettleTMChunkedQueue :: Int -- ^ settle period in microseconds
                             -> TMChunkedQueue a
                             -> IO (Maybe (ChunkedQueue a))
drainAndSettleTMChunkedQueue delay queue = do
    maybeChQueue <- atomically $ drainTMChunkedQueue queue
    case maybeChQueue of
        Nothing -> return Nothing
        Just (ChunkedQueue chunks) -> Just <$> go chunks

    where
        go acc = do
            threadDelay delay

            let terminate = return $ ChunkedQueue acc

            maybeChQueue <- atomically $ tryDrainTMChunkedQueue queue
            case maybeChQueue of
                Nothing -> terminate
                Just (ChunkedQueue []) -> terminate
                Just (ChunkedQueue chunks) -> go (chunks ++ acc)


-- | Keep draining the queue for at least the specified time period. Blocks if
-- the queue is empty to begin with, and starts timing as soon as the first
-- value appears in the queue.
drainWithTimeoutTMChunkedQueue :: Int -- ^ timeout in microseconds
                               -> TMChunkedQueue a
                               -> IO (Maybe (ChunkedQueue a))
drainWithTimeoutTMChunkedQueue delay queue = do
    stashedQueue <- newTChunkedQueueIO

    let transferItems = atomically $ do
        maybeItems <- drainTMChunkedQueue queue
        case maybeItems of
            Nothing -> return Nothing
            Just items -> Just <$> stashedQueue `writeManyTChunkedQueue` consumeQueue items

    let transferTask = do
        result <- transferItems
        case result of
            Nothing -> return ()
            Just _ -> transferTask

    result <- transferItems -- run transfer once before timing, so we block on empty queue.
    case result of
        Nothing -> return Nothing
        Just _ -> do
            withTimeout delay transferTask
            Just <$> atomically (drainTChunkedQueue stashedQueue)

    where 
        withTimeout t action = void $ action `race` threadDelay t


----------------------------------------------------------------
