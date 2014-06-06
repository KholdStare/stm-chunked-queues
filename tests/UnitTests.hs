module Main (main) where

import Test.Tasty
import qualified TChunkedQueue
import qualified TMChunkedQueue

main :: IO ()
main = defaultMain $
    testGroup "STM ChunkedQueues"
        [ testGroup "TChunkedQueue" TChunkedQueue.tests
        , testGroup "TMChunkedQueue" TMChunkedQueue.tests
        ]
