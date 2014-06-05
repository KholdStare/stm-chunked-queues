module Control.Concurrent.STM.ChunkedQueue (

    -- * Simple Chunked Queue
    ChunkedQueue(..),
    consumeQueue,
    enqueueOne,
    enqueueMany,

) where


import Data.Monoid

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

