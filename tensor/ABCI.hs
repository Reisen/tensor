module ABCI
  ( abciCheckTx
  , abciDeliverTx
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

import           Data.Aeson                 ( FromJSON(..), ToJSON(..), encode, eitherDecode )
import           Data.Char                  ( Char )

import qualified Data.Base58String.Bitcoin as B58

import           FFI                        ( Context, getContext, registerCallbacks, executeWASM )
import           Types                      ( Tensor(..) )



--------------------------------------------------------------------------------



data Message
  = RegisterContract   Text
  | ExecuteTransaction Text
  deriving (Generic, Eq)

instance FromJSON Message where
instance ToJSON   Message where



--------------------------------------------------------------------------------



abciCheckTx
  :: ByteString
  -> Tensor
  -> IO Tensor

abciCheckTx transaction tensor@Tensor{..} = do
  putText "Handling Tensor DeliverTx"
  pure tensor
    { txCounter = txCounter + 1
    }


abciDeliverTx
  :: ByteString
  -> Tensor
  -> IO Tensor

abciDeliverTx transaction tensor@Tensor{..} = do
  putText "Handling Tensor DeliverTx"
  case eitherDecode (toS transaction) of
    Left _  -> pure tensor
    Right v -> case v of
      ExecuteTransaction t -> executeTransaction t *> pure tensor
      RegisterContract c   -> pure tensor


executeTransaction
  :: Text
  -> IO ()

executeTransaction bytes = do
  let decoded = B58.toBytes . B58.fromText $ bytes
  putText "--------------------------------------------------------------------------------"
  putText (toS decoded)
  putText ("Correct: " <> show (decoded == "\x00\x61\x73\x6d\x01\x00\x00\x00\x01\x06\x01\x60\x01\x7f\x01\x7f\x03\x02\x01\x00\x07\x0b\x01\x07\x61\x64\x64\x5f\x6f\x6e\x65\x00\x00\x0a\x09\x01\x07\x00\x20\x00\x41\x01\x6a\x0b\x00\x1a\x04\x6e\x61\x6d\x65\x01\x0a\x01\x00\x07\x61\x64\x64\x5f\x6f\x6e\x65\x02\x07\x01\x00\x01\x00\x02\x70\x30"))
  putText "--------------------------------------------------------------------------------"
  executeWASM decoded


abciCommit :: Tensor -> IO Tensor
abciCommit tensor@Tensor{..} = do
  putText     "Handling Tensor Commit"
  executeWASM "\x00\x61\x73\x6d\x01\x00\x00\x00\x01\x06\x01\x60\x01\x7f\x01\x7f\x03\x02\x01\x00\x07\x0b\x01\x07\x61\x64\x64\x5f\x6f\x6e\x65\x00\x00\x0a\x09\x01\x07\x00\x20\x00\x41\x01\x6a\x0b\x00\x1a\x04\x6e\x61\x6d\x65\x01\x0a\x01\x00\x07\x61\x64\x64\x5f\x6f\x6e\x65\x02\x07\x01\x00\x01\x00\x02\x70\x30"
  pure tensor


abciWorker :: IO ()
abciWorker = do
  let transaction = B58.toText . B58.fromBytes . toS . encode . ExecuteTransaction . B58.toText . B58.fromBytes $ "\x00\x61\x73\x6d\x01\x00\x00\x00\x01\x06\x01\x60\x01\x7f\x01\x7f\x03\x02\x01\x00\x07\x0b\x01\x07\x61\x64\x64\x5f\x6f\x6e\x65\x00\x00\x0a\x09\x01\x07\x00\x20\x00\x41\x01\x6a\x0b\x00\x1a\x04\x6e\x61\x6d\x65\x01\x0a\x01\x00\x07\x61\x64\x64\x5f\x6f\x6e\x65\x02\x07\x01\x00\x01\x00\x02\x70\x30"
  putText (toS transaction)
  ctx <- getContext
  registerCallbacks ctx
    abciCheckTx
    abciDeliverTx
    abciCommit
