module Sample
  ( drawTable
  , sampleT
  , sampleW
  , sampleU
  ) where

import Data.Maybe

import System.Random.MWC
import System.Random.MWC.CondensedTable

import qualified Data.Vector.Unboxed as U

------------------------------------------------------------------------

-- | Compute a 'CondensedTableU' for drawing indices into a vector that
-- represents the \"population\".
--
-- That is, @drawTable n w@ returns a 'CondensedU' table for
-- @0..n-1@ where @n@ is the population size and @w@ are the probability
-- weights.
--
-- If no probability weights are supplied, all indices have equal
-- probability of being drawn.
--
-- Warning: the table will take up a lot of memory for large populations.
drawTable :: Int                     -- ^ Population size
          -> Maybe (U.Vector Double) -- ^ Probability weights.
          -> CondensedTableU Int
drawTable n = tableFromProbabilities . U.zip (U.enumFromN 0 n)
            . fromMaybe (U.replicate n (1 / fromIntegral n))
{-# INLINE drawTable #-}

------------------------------------------------------------------------

-- | Perform a sample using a pre-computed 'CondensedTable'.
--
-- This defintion is provided so that multiple samples may be
-- drawn from the same table within a single session, possibly
-- saving some work.
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

-- | Perform a weighted sample.
--
-- Note: unless a weighted sample is desired, use 'sampleU' instead.
-- The two are similar in performance, but 'sampleW' will not handle
-- samples from very large populations that well (see 'drawTable').
sampleW :: U.Unbox e => U.Vector e
        -> Int
        -> Maybe (U.Vector Double)
        -> GenIO
        -> IO (U.Vector e)
sampleW x size probs g = sampleT x size (drawTable (U.length x) probs) g
{-# INLINE sampleW #-}

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
