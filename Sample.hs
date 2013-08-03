module Sample
  ( drawTableP
  , drawTableW
  , drawTableI
  , sampleT
  , sampleP
  , sampleW
  , sampleI
  , sampleU
  ) where

import Data.Maybe
import Data.Word

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

drawTable' :: (Num e, U.Unbox e, U.Unbox w)
           => (U.Vector (e, w) -> CondensedTableU Int)
           -> w
           -> Int
           -> Maybe (U.Vector w)
           -> CondensedTableU Int
drawTable' mk d n = mk . U.zip (U.enumFromN 0 n)
                  . fromMaybe (U.replicate n d)
{-# INLINE drawTable' #-}

-- | Produce a 'CondensedTableU' for probability weighted sampling of
-- population indices.
drawTableP :: Int                     -- ^ Population size
          -> Maybe (U.Vector Double)  -- ^ Probability weights.
          -> CondensedTableU Int
drawTableP n = drawTable' tableFromProbabilities (1/fromIntegral n) n
{-# INLINE drawTableP #-}

-- | Produce a 'CondensedTableU' for weighted sampling of population indices.
drawTableW :: Int                     -- ^ Population size
          -> Maybe (U.Vector Double)  -- ^ Weights
          -> CondensedTableU Int
drawTableW n = drawTable' tableFromWeights (1/fromIntegral n) n
{-# INLINE drawTableW #-}

-- | Produce a 'CondensedTableU' for weighted sampling of population indices.
drawTableI :: Int                     -- ^ Population size
          -> Maybe (U.Vector Word32)  -- ^ Integer weights
          -> CondensedTableU Int
drawTableI n = drawTable' tableFromIntWeights w n
  where w = fromIntegral $ (2^(32::Int)) `div` n
{-# INLINE drawTableI #-}

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

sampleW' :: U.Unbox e => U.Vector e
         -> Int
         -> w
         -> (Int -> w -> CondensedTableU Int)
         -> GenIO
         -> IO (U.Vector e)
sampleW' x size ws mkTable g = sampleT x size (mkTable (U.length x) ws) g
{-# INLINE sampleW' #-}

-- | Perform a sample using probability weights.
sampleP :: U.Unbox e => U.Vector e
        -> Int
        -> Maybe (U.Vector Double)
        -> GenIO
        -> IO (U.Vector e)
sampleP x size probs g = sampleW' x size probs drawTableP g
{-# INLINE sampleP #-}

-- | Perform a sample using weights.
sampleW :: U.Unbox e => U.Vector e
        -> Int
        -> Maybe (U.Vector Double)
        -> GenIO
        -> IO (U.Vector e)
sampleW x size ws g = sampleW' x size ws drawTableW g
{-# INLINE sampleW #-}

-- | Perform a sample using integer weights.
sampleI :: U.Unbox e => U.Vector e
        -> Int
        -> Maybe (U.Vector Word32)
        -> GenIO
        -> IO (U.Vector e)
sampleI x size ws g = sampleW' x size ws drawTableI g
{-# INLINE sampleI #-}

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
