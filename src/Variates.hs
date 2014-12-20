{-|
Module : Variates

Thin wrappers over "System.Random.MWC" for generating
random vectors, modelled after the API in GNU R.

These definitions serve no purpose other than to possibly
save a few keystrokes when used interactively, compared to
using "System.Random.MWC" directly.
-}

module Variates
  ( variates
  , variatesU
  , rbinom
  , runif, runif'
  , rnorm, rnorm'
  ) where

import Data.Maybe (fromMaybe)

import Control.Monad.Primitive

import System.Random.MWC
import System.Random.MWC.CondensedTable
import System.Random.MWC.Distributions

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

------------------------------------------------------------------------

rbinom :: (PrimMonad m) => Int    -- ^ Number of obs.
       -> Int    -- ^ Number of trials
       -> Double -- ^ Probability of success
       -> Gen (PrimState m)
       -> m (U.Vector Int)
rbinom n siz prob = variates n $ genFromTable (tableBinomial siz prob)

------------------------------------------------------------------------

runif :: (U.Unbox e, Bounded e, Variate e, PrimMonad m) => Int
      -> Gen (PrimState m) -> m (U.Vector e)
runif n = runif' n (minBound, maxBound)

runif' :: (U.Unbox e, Variate e, PrimMonad m) => Int -> (e, e)
       -> Gen (PrimState m)
       -> m (U.Vector e)
runif' n = variates n . uniformR

------------------------------------------------------------------------

rnorm :: (PrimMonad m) => Int -> Gen (PrimState m)
      -> m (U.Vector Double)
rnorm n = rnorm' n Nothing

rnorm' :: (PrimMonad m) => Int -> Maybe (Double, Double)
       -> Gen (PrimState m) -> m (U.Vector Double)
rnorm' n = variates n . uncurry normal . fromMaybe (0, 1)

------------------------------------------------------------------------

variates :: (G.Vector v e, PrimMonad m) => Int
         -> (Gen (PrimState m) -> m e) -> Gen (PrimState m) -> m (v e)
variates n f g = G.generateM n (const $ f g)
{-# INLINE variates #-}

------------------------------------------------------------------------

variatesU :: (U.Unbox e, PrimMonad m) => Int
          -> (Gen (PrimState m) -> m e) -> Gen (PrimState m)
          -> m (U.Vector e)
variatesU = variates
{-# INLINE variatesU #-}
