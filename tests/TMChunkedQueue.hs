{-# LANGUAGE DoAndIfThenElse #-}
module TMChunkedQueue (
    tests
) where

{-import Control.Applicative ((<$>))-}
import Control.Monad.STM   (atomically)
import Data.Monoid

import Control.Concurrent.STM.TMChunkedQueue

import TestUtils
import qualified TChunkedQueue

import Test.Tasty
import Test.Tasty.HUnit


-- | Convenience specialization to avoid "Default Constraint" warnings
testActions' :: [QueueAction Int] -> [Maybe [Int]] -> Assertion
testActions' = testActions $
        TestableChunkedQueue
            newTMChunkedQueueIO
            (drainAndSettleTMChunkedQueue 20000)
            ((fmap collapseMaybe) . atomically . tryDrainTMChunkedQueue)
            writeTMChunkedQueue
            closeTMChunkedQueue

    where
        collapseMaybe Nothing = mempty
        collapseMaybe (Just xs) = xs


tests :: [TestTree]
tests = 
    [ testGroup "Non-Closing" $ TChunkedQueue.testWith testActions' ]
