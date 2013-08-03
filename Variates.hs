module Variates
  ( variates
  , variatesU
  , rbinom
  , runif, runif'
  , rnorm, rnorm'
  ) where

import Data.Maybe (fromMaybe)

import System.Random.MWC
import System.Random.MWC.CondensedTable
import System.Random.MWC.Distributions

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

------------------------------------------------------------------------

rbinom :: Int    -- ^ Number of obs.
       -> Int    -- ^ Number of trials
       -> Double -- ^ Probability of success
       -> GenIO
       -> IO (U.Vector Int)
rbinom n siz prob = variates n $ genFromTable (tableBinomial siz prob)

------------------------------------------------------------------------

runif :: Int -> GenIO -> IO (U.Vector Double)
runif n = runif' n Nothing

runif' :: Int -> Maybe (Double, Double) -> GenIO -> IO (U.Vector Double)
runif' n = variates n . uniformR . fromMaybe (0, 1)

------------------------------------------------------------------------

rnorm :: Int -> GenIO -> IO (U.Vector Double)
rnorm n = rnorm' n Nothing

rnorm' :: Int -> Maybe (Double, Double) -> GenIO -> IO (U.Vector Double)
rnorm' n = variates n . uncurry normal . fromMaybe (0, 1)

------------------------------------------------------------------------

variates :: G.Vector v e => Int -> (GenIO -> IO e) -> GenIO -> IO (v e)
variates n f g = G.generateM n (const $ f g)
{-# INLINE variates #-}

------------------------------------------------------------------------

variatesU :: U.Unbox e => Int -> (GenIO -> IO e) -> GenIO
          -> IO (U.Vector e)
variatesU = variates
{-# INLINE variatesU #-}
