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

import Control.Monad.ST (ST)

import System.Random.MWC
import System.Random.MWC.CondensedTable
import System.Random.MWC.Distributions

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

------------------------------------------------------------------------

rbinom :: Int    -- ^ Number of obs.
       -> Int    -- ^ Number of trials
       -> Double -- ^ Probability of success
       -> Gen s
       -> ST s (U.Vector Int)
rbinom n siz prob = variates n $ genFromTable (tableBinomial siz prob)

------------------------------------------------------------------------

runif :: Int -> Gen s -> ST s (U.Vector Double)
runif n = runif' n Nothing

runif' :: Int -> Maybe (Double, Double) -> Gen s -> ST s (U.Vector Double)
runif' n = variates n . uniformR . fromMaybe (0, 1)

------------------------------------------------------------------------

rnorm :: Int -> Gen s -> ST s (U.Vector Double)
rnorm n = rnorm' n Nothing

rnorm' :: Int -> Maybe (Double, Double) -> Gen s -> ST s (U.Vector Double)
rnorm' n = variates n . uncurry normal . fromMaybe (0, 1)

------------------------------------------------------------------------

variates :: G.Vector v e => Int -> (Gen s -> ST s e) -> Gen s
         -> ST s (v e)
variates n f g = G.generateM n (const $ f g)
{-# INLINE variates #-}

------------------------------------------------------------------------

variatesU :: U.Unbox e => Int -> (Gen s -> ST s e) -> Gen s
          -> ST s (U.Vector e)
variatesU = variates
{-# INLINE variatesU #-}
