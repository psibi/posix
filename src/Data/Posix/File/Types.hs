module Data.Posix.File.Types where

import Data.Int (Int64)

-- | Haskell type representing the C off_t type
newtype OffT = OffT
  { unOffT :: Int64
  } deriving (Show, Eq, Ord)
