module Types.Tensor
  ( Tensor(..)
  , initialTensor
  ) where

-- Imports Grouped By:
--
-- o Imports from `base` or `Prelude` (protolude) only.
-- o Imports from external packages.
-- o Qualified Imports.
-- o Project Imports.

import           Protolude

--------------------------------------------------------------------------------



-- | Application State

data Tensor = Tensor
  { txCounter :: Int
  } deriving (Eq, Show)

initialTensor :: Tensor
initialTensor = Tensor
  { txCounter = 0
  }
