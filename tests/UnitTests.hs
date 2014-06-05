module Main (main) where

import Test.Tasty
import qualified TChunkedQueue

main :: IO ()
main = defaultMain $
    testGroup "STM TChunkedQueues"
        [ testGroup "TChunkedQueue" TChunkedQueue.tests ]
