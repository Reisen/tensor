{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}

module FFI
  ( Context
  , getContext
  , registerCallbacks
  , executeWASM
  ) where

-- Imports Grouped By:
--
-- o Imports from `base` or `Prelude` (protolude) only.
-- o Imports from external packages.
-- o Qualified Imports.
-- o Project Imports.

import           Protolude               hiding ( hash )
import           Foreign.Ptr                    ( FunPtr, Ptr, nullPtr )
import           Foreign.C.Types                ( CChar )
import           Foreign.C.String               ( CString )

import           Data.ByteString                ( useAsCStringLen, packCStringLen )

import           Types                          ( Tensor, initialTensor )



--------------------------------------------------------------------------------



-- | Define an Opaque type that we can use as a pointer type. ABCIContext comes
-- | from rust land. We wrap it up in a `Context` here in Haskell land so that
-- | the type isn't exposed to the rest of the codebase.

data ABCIContext

newtype Context = Context_
  { abciContext :: Ptr ABCIContext
  }



--------------------------------------------------------------------------------



-- Define Callback Types

type CheckTxCallback   = Ptr CChar -> Int -> IO ()
type DeliverTxCallback = Ptr CChar -> Int -> IO ()
type CommitCallback    = IO ()
type Wrapper f         = f -> IO (FunPtr f)


-- Define FFI "wrappers" for all the Haskell functions we want to expose and
-- pass out to Rust/C code.

foreign import ccall "wrapper" checkTxCallback   :: Wrapper CheckTxCallback
foreign import ccall "wrapper" deliverTxCallback :: Wrapper DeliverTxCallback
foreign import ccall "wrapper" commitCallback    :: Wrapper CommitCallback


-- Define FFI calls for all the functions we want to import from C/Rust land
-- and call from Haskell land.

foreign import ccall "execute_wasm" execute_wasm :: CString -> Int -> IO ()
foreign import ccall "get_abci_context" get_context :: IO (Ptr ABCIContext)
foreign import ccall "register_abci_callback" register_callback
  :: Ptr ABCIContext
  -> FunPtr (Ptr CChar -> Int -> IO ())
  -> FunPtr (Ptr CChar -> Int -> IO ())
  -> FunPtr (IO ())
  -> IO ()



--------------------------------------------------------------------------------



-- | Rust land creates a Context using rust-abci, which handles all the connection
-- | management with Tendermint. This function just pulls out a Ptr so we can use
-- | it as a Context to our Haskell handlers.

getContext :: IO Context
getContext = map Context_ get_context


-- | Note: Does Not Return

registerCallbacks
  :: Context                             -- ^ Tensor Context Pointer
  -> (ByteString -> Tensor -> IO Tensor) -- ^ CheckTxCallback
  -> (ByteString -> Tensor -> IO Tensor) -- ^ DeliverTxCallback
  -> (Tensor -> IO Tensor)               -- ^ CommitCallback
  -> IO ()                               -- ^ Side Effectful

registerCallbacks (Context_ abci) checkTx deliverTx commit = do
  -- Serialize bytes from Rust land into a ByteString that our callbacks can
  -- receive/handle.
  tensor <- newMVar initialTensor
  let serializer cb = curry $ (>>= modifyMVar_ tensor . cb) . packCStringLen

  -- Create Callbacks
  rust_checkTx   <- checkTxCallback   (serializer checkTx)
  rust_deliverTx <- deliverTxCallback (serializer deliverTx)
  rust_commit    <- commitCallback    (modifyMVar_ tensor commit)

  -- Register Tensor Callback
  register_callback abci
    rust_checkTx
    rust_deliverTx
    rust_commit


executeWASM :: ByteString -> IO ()
executeWASM = flip useAsCStringLen (uncurry execute_wasm)
