module Variates
  ( variates
  , variatesU
  ) where

import System.Random.MWC
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

------------------------------------------------------------------------

variates :: G.Vector v e => Int
         -> (GenIO -> IO e)
         -> GenIO
         -> IO (v e)
variates n f g = G.generateM n $ \_ -> do
  e <- f g
  return $! e

------------------------------------------------------------------------

variatesU :: U.Unbox e => Int
          -> (GenIO -> IO e)
          -> GenIO
          -> IO (U.Vector e)
variatesU = variates
