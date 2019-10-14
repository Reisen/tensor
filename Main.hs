#!/usr/bin/env stack
-- stack runghc --package=protolude --package=cryptonite --package=base58string --package=memory



--------------------------------------------------------------------------------



{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

-- Imports Grouped By:
--
-- o Imports from `base` or `Prelude` (protolude) only.
-- o Imports from external packages.
-- o Qualified Imports

import           Protolude          hiding ( hash )
import           Data.Typeable             ( typeOf )

import           Data.ByteArray            ( convert )
import           Data.Base58String.Bitcoin ( toText, fromBytes )
import           Crypto.Hash               ( Digest, hash )
import           Crypto.Hash.Algorithms    ( HashAlgorithm, SHA256 )

import qualified Data.Text                as T



--------------------------------------------------------------------------------



-- | A "Block" in this system is similar to a PoH slice from Solana, we use a
-- | hash chain so that we can properly order and refer to transactions with
-- | transactions as dependencies.

data HashChain hash = HashChain
  { currentHash  :: Digest hash
  , counter      :: Int
  }


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
  => Int              -- ^ How many hashes to advance by.
  -> Maybe ByteString -- ^ Data to mix into the chain.
  -> HashChain h      -- ^ Target Chain
  -> HashChain h      -- ^ Updated Chain

update n mayInput chain@HashChain{..} = case mayInput of
  -- When mixing an input into the chain, we advance by a single hash and hash
  -- the next hash with the input mixed into the bytes.
  Just input -> chain
    { counter      = counter + 1
    , currentHash  = currentHash
        & hash @ByteString @h
        . mappend input
        . show
        . hash @ByteString @h
        . show
    }

  -- On the other hand when no data is being mixed in, we simply hash n times
  -- mod the increment resolution.
  Nothing    -> chain
    { counter      = counter + (fromIntegral $ n - (counter `mod` n))
    , currentHash  = currentHash
        & hash @ByteString @h
        . show
        . applyN n (hash @ByteString @h . show)
    }



-- | This just renders the current state in terms of Hash & Counter.

renderChain
  :: forall h. Typeable h -- ^ For Type Reflection on the Hash
  => HashChain h          -- ^ The chain to render.
  -> IO ()                -- ^ So we can print.

renderChain HashChain{..} = putText $ fold
  [ show . typeRep $ Proxy @h
  , " : " , T.take 9 . toText . fromBytes . convert $ currentHash
  , " - " , show counter
  ]



--------------------------------------------------------------------------------



sha256chain :: HashChain SHA256
sha256chain = HashChain
  { currentHash  = hash ("0" :: ByteString)
  , counter      = 0
  }



--------------------------------------------------------------------------------



main :: IO ()
main = do
  let initialChain = applyN 8 (update 1024 Nothing) sha256chain
  let updateChain0 = update 1024 (Just "Hello") initialChain
  let updateChain1 = update 1024 (Just "Hello") updateChain0
  let updateChain2 = update 1024 (Just "Hello") updateChain1
  let updateChain3 = update 1024 (Just "Hello") updateChain2
  let updateChain4 = update 1024 (Just "Hello") updateChain3
  let updateChain5 = update 1024 (Just "Hello") updateChain4
  let latestChain  = update 1024 Nothing updateChain5
  renderChain initialChain
  renderChain updateChain5
  renderChain latestChain
