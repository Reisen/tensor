module Main where

-- Imports Grouped By:
--
-- o Imports from `base` or `Prelude` (protolude) only.
-- o Imports from external packages.
-- o Qualified Imports.
-- o Project Imports.

import           Protolude               hiding ( hash )

import           Control.Concurrent.Async       ( waitAnyCancel )
import           ABCI                           ( abciWorker )
import           FFI                            ( Context, getContext, registerCallbacks )
import           Types                          ( Tensor(..)
                                                , HashChain(..)
                                                , update
                                                , renderChain
                                                , sha256chain
                                                )



--------------------------------------------------------------------------------



transactionWorker :: IO ()
transactionWorker = do
  pure ()


main :: IO ()
main = do
  let initialChain = applyN 8 (update Nothing) sha256chain
  let updateChain0 = update (Just "Hello") initialChain
  let updateChain1 = update (Just "Hello") updateChain0
  let updateChain2 = update (Just "Hello") updateChain1
  let updateChain3 = update (Just "Hello") updateChain2
  let updateChain4 = update (Just "Hello") updateChain3
  let updateChain5 = update (Just "Hello") updateChain4
  let latestChain  = update Nothing updateChain5
  renderChain initialChain
  renderChain updateChain5
  renderChain latestChain
  abciWorker
