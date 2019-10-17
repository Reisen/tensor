module ABCI
  ( abciDeliverTx
  , abciCheckTx
  , abciCommit
  , abciWorker
  ) where

-- Imports Grouped By:
--
-- o Imports from `base` or `Prelude` (protolude) only.
-- o Imports from external packages.
-- o Qualified Imports.
-- o Project Imports.

import           Protolude

import           FFI                            ( Context, getContext, registerCallbacks )
import           Types                          ( Tensor(..) )



--------------------------------------------------------------------------------



abciDeliverTx
  :: ByteString
  -> Tensor
  -> IO Tensor

abciDeliverTx transaction tensor@Tensor{..} = do
  putText "Handling Tensor DeliverTx"
  pure tensor
    { txCounter = txCounter + 1
    }


abciCheckTx
  :: ByteString
  -> Tensor
  -> IO Tensor

abciCheckTx transaction tensor@Tensor{..} = do
  putText "Handling Tensor DeliverTx"
  pure tensor
    { txCounter = txCounter + 1
    }


abciCommit :: Tensor -> IO Tensor
abciCommit tensor@Tensor{..} = do
  putText "Handling Tensor Commit"
  pure tensor
    { txCounter = txCounter + 1
    }


abciWorker :: IO ()
abciWorker = do
  ctx <- getContext
  registerCallbacks ctx
    abciCheckTx
    abciDeliverTx
    abciCommit
