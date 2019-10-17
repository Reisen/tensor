#!/usr/bin/env stack
-- stack runghc --package=protolude --package=cryptonite --package=base58string --package=memory



--------------------------------------------------------------------------------



{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}

module Main where

-- Imports Grouped By:
--
-- o Imports from `base` or `Prelude` (protolude) only.
-- o Imports from external packages.
-- o Qualified Imports

import           Protolude               hiding ( hash )
import           Data.Typeable                  ( typeOf )
import           Foreign.Ptr                    ( FunPtr, Ptr, nullPtr )
import           Foreign.C.Types                ( CChar )
import           Foreign.C.String               ( CString )

import           Control.Lens                   ( makeLenses )
import           Control.Lens.Operators
import           Data.ByteArray                 ( convert )
import           Data.ByteString                ( packCStringLen )
import           Data.Base58String.Bitcoin      ( toText, fromBytes )
import           Crypto.Hash                    ( Digest, hash )
import           Crypto.Hash.Algorithms         ( HashAlgorithm, SHA256 )
import           Control.Concurrent.Async       ( waitAnyCancel )

import qualified Data.Text                     as T
import qualified Criterion                     as Criterion
import qualified Criterion.Main                as Criterion



--------------------------------------------------------------------------------



-- Before we define anything, here we define our C interface with the ABCI shim.
-- The ABCI shim provides us with access to Tendermint core. This eliminates the
-- consensus pain for this project.
--
-- Might eventually want to replace this with fork-selection style consensus ala
-- Bitcoin/Solana but for now this gets us bootstrapped.

data ABCIContext

newtype Context = Context
  { abciContext :: Ptr ABCIContext
  }

-- We need to define some magical wrappers so that we can convert Haskell
-- callbacks into C function prototypes.
type CheckTxCallback = Ptr CChar -> Int -> IO ()
foreign import ccall "wrapper" checkTxCallback
  :: CheckTxCallback
  -> IO (FunPtr CheckTxCallback)

type DeliverTxCallback = Ptr CChar -> Int -> IO ()
foreign import ccall "wrapper" deliverTxCallback
  :: DeliverTxCallback
  -> IO (FunPtr DeliverTxCallback)

type CommitCallback = IO ()
foreign import ccall "wrapper" commitCallback
  :: IO ()
  -> IO (FunPtr CommitCallback)

-- Define Foreign Function Signatures
foreign import ccall "execute_wasm" execute_wasm :: CString -> IO ()
foreign import ccall "get_abci_context" get_context :: IO (Ptr ABCIContext)
foreign import ccall "register_abci_callback" register_callback
  :: Ptr ABCIContext
  -> FunPtr (Ptr CChar -> Int -> IO ())
  -> FunPtr (Ptr CChar -> Int -> IO ())
  -> FunPtr (IO ())
  -> IO ()

-- Implement External C Functions
getContext :: IO Context
getContext = map Context get_context

registerCallbacks
  :: Context                             -- ^ Tensor Context Pointer
  -> (ByteString -> Tensor -> IO Tensor) -- ^ CheckTxCallback
  -> (ByteString -> Tensor -> IO Tensor) -- ^ DeliverTxCallback
  -> (Tensor -> IO Tensor)               -- ^ CommitCallback
  -> IO ()                               -- ^ Side Effectful

registerCallbacks (Context abci) checkTx deliverTx commit = do
  putText "[haskell] Registering..."

  tensor         <- newMVar initialTensor
  let serializer cb b l = packCStringLen (b, l) >>= modifyMVar_ tensor . cb

  -- Create Callbacks
  rust_checkTx   <- checkTxCallback (serializer checkTx)
  rust_commit    <- commitCallback $ modifyMVar_ tensor commit
  rust_deliverTx <- deliverTxCallback (serializer deliverTx)

  -- Register Tensor Callback
  putText "[haskell] Done"
  register_callback abci rust_checkTx rust_deliverTx rust_commit



--------------------------------------------------------------------------------


-- | Application State

data Tensor = Tensor
  { txCounter :: Int
  } deriving (Eq, Show)

initialTensor :: Tensor
initialTensor = Tensor
  { txCounter = 0
  }



-- | A "Block" in this system is similar to a PoH slice from Solana, we use a
-- | hash chain so that we can properly order and refer to transactions with
-- | transactions as dependencies.

data HashChain hash = HashChain
  { _counter     :: Int
  , _currentHash :: Digest hash
  , _resolution  :: Int
  }

makeLenses ''HashChain


-- | Update the HashChain with data to mix into it. Data could eithre be other
-- | transactions or basically any data at all that you want to mix into the
-- | stream. The acting of mixing acts as a proof of existence at some point in
-- | time.
-- |
-- | This function requires _serious_ optimization for this to work, if we're
-- | not maximizing hash throughput we risk attacks.

update
  :: forall h.
     HashAlgorithm h  -- ^ Require a Cryptonite compatible hashing algorithm
  => Maybe ByteString -- ^ Data to mix into the chain.
  -> HashChain h      -- ^ Target Chain
  -> HashChain h      -- ^ Updated Chain

update mayInput chain@HashChain{..} = case mayInput of
  -- When mixing an input into the chain, we advance by a single hash and hash
  -- the next hash with the input mixed into the bytes.
  Just input -> chain
    & counter     +~ 1
    & currentHash %~
          hash @ByteString @h
        . mappend input
        . show
        . hash @ByteString @h
        . show

  -- On the other hand when no data is being mixed in, we simply hash n times
  -- mod the increment _resolution.
  Nothing    -> chain
    & counter     +~ _resolution - (_counter `mod` _resolution)
    & currentHash %~ applyN _resolution (hash @ByteString @h . show)


-- | This just renders the current state in terms of Hash & Counter.

renderChain
  :: forall h. Typeable h -- ^ For Type Reflection on the Hash
  => HashChain h          -- ^ The chain to render.
  -> IO ()                -- ^ So we can print.

renderChain HashChain{..} = putText $ fold
  [ show . typeRep $ Proxy @h
  , " : " , T.take 9 . toText . fromBytes . convert $ _currentHash
  , " - " , show _counter
  ]


-- | Benchmark Chains

benchmarkChain :: HashAlgorithm h => HashChain h -> IO ()
benchmarkChain chain = Criterion.defaultMain
  [ Criterion.bgroup "sha256chain"
      [ Criterion.bench "Nothing: 1024"
          $ Criterion.whnf (update Nothing) chain
      , Criterion.bench "Nothing:    1"
          $ Criterion.whnf (hash @ByteString @SHA256) ("foo" :: ByteString)
      ]
  ]


--------------------------------------------------------------------------------



sha256chain :: HashChain SHA256
sha256chain = HashChain
  { _currentHash = hash ("0" :: ByteString)
  , _resolution  = 1024
  , _counter     = 0
  }



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
