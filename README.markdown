# stm-chunked-queues [![Build Status](https://travis-ci.org/KholdStare/stm-chunked-queues.svg?branch=master)](https://travis-ci.org/KholdStare/stm-chunked-queues)

Thread communication queues that group items/requests that occur close together
in time.

## Example

Chunked queues allow grouping together items that may be too granular to
process one-by-one. In the following example, the listening thread receives
chunks of items by allowing the queue to settle for 2 ms.

```haskell
import  Control.Concurrent
import  Control.Concurrent.Async
import  Control.Concurrent.STM
import  Control.Concurrent.STM.TMChunkedQueue


-- | delay in microseconds
millisecond :: Int
millisecond = 1000


main :: IO ()
main = do
    queue <- newTMChunkedQueueIO
    -- start listening thread
    finished <- async $ (2 * millisecond) `settleAndPrint` queue

    let enqueue = atomically . writeManyTMChunkedQueue queue

    -- queue will group enqueues below
    enqueue [1, 2]
    enqueue [3, 4]
    threadDelay millisecond
    enqueue [5, 6]

    -- long millisecond so listening thread settles
    threadDelay (4 * millisecond)

    -- listening thread will start new group
    enqueue [7, 8]

    atomically $ closeTMChunkedQueue queue
    wait finished


-- | repeatedly drains a queue with a settle period, and prints the resulting
-- chunks
settleAndPrint :: Int -> TMChunkedQueue Int -> IO ()
settleAndPrint delay queue = go

    where go = do
            maybeItems <- drainAndSettleTMChunkedQueue delay queue

            case maybeItems of
                Nothing -> return () -- queue closed
                Just items -> print (consumeQueue items) >> go
```

Which print the following:

```
[1,2,3,4,5,6]
[7,8]
```
