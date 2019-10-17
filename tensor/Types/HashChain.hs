module Types.HashChain
  ( HashChain(..)
  , update
  , renderChain
  , sha256chain

  -- Lens Exports
  , counter
  , currentHash
  , resolution
  ) where

-- Imports Grouped By:
--
-- o Imports from `base` or `Prelude` (protolude) only.
-- o Imports from external packages.
-- o Qualified Imports.
-- o Project Imports.

import           Protolude               hiding ( hash )

import           Control.Lens                   ( makeLenses )
import           Control.Lens.Operators
import           Crypto.Hash.Algorithms         ( HashAlgorithm, SHA256 )
import           Crypto.Hash                    ( Digest, hash )
import           Data.Base58String.Bitcoin      ( toText, fromBytes )
import           Data.ByteArray                 ( convert )

import qualified Criterion                     as Criterion
import qualified Criterion.Main                as Criterion
import qualified Data.Text                     as T

--------------------------------------------------------------------------------



-- | A "Block" in this system is similar to a PoH slice from Solana, we use a
-- | hash chain so that we can properly order and refer to transactions with
-- | transactions as dependencies.

data HashChain hash = HashChain
  { _counter     :: Int
  , _currentHash :: Digest hash
  , _resolution  :: Int
  }

sha256chain :: HashChain SHA256
sha256chain = HashChain
  { _currentHash = hash ("0" :: ByteString)
  , _resolution  = 1024
  , _counter     = 0
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



--------------------------------------------------------------------------------



-- | Benchmark Chains

benchmarkChain :: HashAlgorithm h => HashChain h -> IO ()
benchmarkChain chain = Criterion.defaultMain
  [ Criterion.bgroup "sha256chain"
      [ Criterion.bench "Nothing: 1024" $ Criterion.whnf (update Nothing) chain
      , Criterion.bench "Nothing:    1" $ Criterion.whnf (hash @ByteString @SHA256) ("foo" :: ByteString)
      ]
  ]
