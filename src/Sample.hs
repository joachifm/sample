{-# LANGUAGE FlexibleContexts #-}

{-|
Module : Sample

Definitions for taking samples, modelled after the API in GNU R.

Note that the following definitions are merely thin wrappers over
"System.Random.MWC", intended for interactive use.
-}

module Sample
  (
    -- * General sampling interface
    sample
    -- * Unweighted sampling
  , sampleU
    -- * Weighted sampling
    -- $weightedsample
  , sampleP
  , sampleW
  , sampleI
  ) where

import Data.Word

import Control.Monad.ST (ST)

import System.Random.MWC
import System.Random.MWC.CondensedTable

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

------------------------------------------------------------------------

-- | A convenience wrapper for taking samples.
--
-- @sample x s m@ generates a vector of size @s@ by drawing @s@
-- elements from @x@ using @m@, where @m@ is a function that generates
-- indices into @x@.
--
-- Indices returned by @m@ are used without bounds checking.
--
-- Taking an unweighted sample:
--
-- @
--   sample x size (uniformR (0, U.length x - 1)) g
-- @
--
-- Taking a probability weighted sample:
--
-- @
--   let tbl = tableFromProbabilities (U.zip (U.fromEnumN 0 (U.length x)) probs)
--   sample x size (genFromTable tbl) g
-- @
sample :: (G.Vector v e) => v e -- ^ Sample space
       -> Int -- ^ Sample size
       -> (Gen s -> ST s Int) -- ^ Generate indices into sample space
       -> Gen s -- ^ Random generator
       -> ST s (v e)
sample x size m g = G.generateM size $ \_ -> (G.unsafeIndexM x =<< m g)
{-# INLINE sample #-}

------------------------------------------------------------------------

-- | A convenience wrapper for unweighted samples.
--
-- Note that if @x@ is a range @[l..u]@, then
--
-- @
--   sampleU x s g = G.generateM s (\\_ -> uniformR (l, u) g)
-- @
sampleU :: G.Vector v e => v e -> Int -> Gen s -> ST s (v e)
sampleU x size g = sample x size (uniformR (0, G.length x - 1)) g
{-# INLINE[0] sampleU #-}

{-# RULES
"sampleU/range" forall a b size g. sampleU (G.enumFromN a b) size g = G.generateM size (\_ -> uniformR (a, b) g)
  #-}

------------------------------------------------------------------------

sampleT :: (U.Unbox w, G.Vector v e)
        => (U.Vector (Int, w) -> CondensedTableU Int)
        -> v e -> Int -> U.Vector w -> Gen s -> ST s (v e)
sampleT m x size ws g = sample x size (genFromTable tbl) g
  where tbl = m (U.zip (U.enumFromN 0 (G.length x)) ws)
{-# INLINE sampleT #-}

------------------------------------------------------------------------
-- $weightedsample
--
-- Convenience wrappers for doing weighted sampling.
--
-- It may be preferable to do
--
-- @
--   let tbl = tableFromProbabilities (U.zip (U.enumFromN 0 (U.length x)) ws)
--   sample x size (genFromTable tbl) g
-- @
--
-- because then the table can be re-used for subsequent samples within the same
-- session.

-- | Probability weighted sampling.
sampleP :: G.Vector v e => v e -> Int -> U.Vector Double -> Gen s
        -> ST s (v e)
sampleP x size probs g = sampleT tableFromProbabilities x size probs g
{-# INLINE sampleP #-}

-- | Weighted sampling.
sampleW :: G.Vector v e => v e -> Int -> U.Vector Double -> Gen s
        -> ST s (v e)
sampleW x size probs g = sampleT tableFromWeights x size probs g
{-# INLINE sampleW #-}

-- | Sampling with integer weights.
sampleI :: G.Vector v e => v e -> Int -> U.Vector Word32 -> Gen s
        -> ST s (v e)
sampleI x size probs g = sampleT tableFromIntWeights x size probs g
{-# INLINE sampleI #-}
