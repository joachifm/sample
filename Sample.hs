module Sample
  ( drawTableP
  , sampleT
  , sampleP
  , sampleU
  ) where

import Data.Maybe

import System.Random.MWC
import System.Random.MWC.CondensedTable

import qualified Data.Vector.Unboxed as U

------------------------------------------------------------------------
-- $drawtable
--
-- For weighted sampling, we compute 'CondensedTableU'S for the
-- population indices.
--
-- That is, @drawTable n w@ returns a 'CondensedU' table for
--- @0..n-1@ where @n@ is the population size and @w@ are the weights.
--
-- If no weights are supplied, all indices are assigned equal weight.

-- | Produce a 'CondensedTableU' for probability weighted sampling of population indices.
drawTableP :: Int                     -- ^ Population size
          -> Maybe (U.Vector Double)  -- ^ Probability weights.
          -> CondensedTableU Int
drawTableP n = tableFromProbabilities . U.zip (U.enumFromN 0 n)
            . fromMaybe (U.replicate n (1 / fromIntegral n))
{-# INLINE drawTableP #-}

------------------------------------------------------------------------

-- | Perform a sample using a pre-computed 'CondensedTable'.
--
-- @genFromTable tbl@ is assumed to return random indices into
-- the supplied population vector @x@.
sampleT :: U.Unbox e => U.Vector e
         -> Int
         -> CondensedTableU Int
         -> GenIO
         -> IO (U.Vector e)
sampleT x size tbl g = U.generateM size $ \_ -> do
  i <- genFromTable tbl g
  return $! (x `U.unsafeIndex` i)
{-# INLINE sampleT #-}

------------------------------------------------------------------------

-- | Perform a sample using probability weights.
sampleP :: U.Unbox e => U.Vector e
        -> Int
        -> Maybe (U.Vector Double)
        -> GenIO
        -> IO (U.Vector e)
sampleP x size probs g = sampleT x size (drawTableP (U.length x) probs) g
{-# INLINE sampleP #-}

------------------------------------------------------------------------

-- | Perform an unweighted sample.
sampleU :: U.Unbox e => U.Vector e
        -> Int
        -> GenIO
        -> IO (U.Vector e)
sampleU x size g = U.generateM size $ \_ -> do
  i <- uniformR (0, n) g
  return $! (x `U.unsafeIndex` i)
  where
    n = U.length x - 1
{-# INLINE sampleU #-}
