{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Numeric.MCMC.Slice
-- Copyright: (c) 2015 Jared Tobin
-- License: MIT
--
-- Maintainer: Jared Tobin <jared@jtobin.ca>
-- Stability: unstable
-- Portability: ghc
--
-- This implementation performs slice sampling by first finding a bracket about
-- a mode (using a simple doubling heuristic), and then doing rejection
-- sampling along it.  The result is a reliable and computationally inexpensive
-- sampling routine.
--
-- The 'mcmc' function streams a trace to stdout to be processed elsewhere,
-- while the `slice` transition can be used for more flexible purposes, such as
-- working with samples in memory.
--
-- See <http://people.ee.duke.edu/~lcarin/slice.pdf Neal, 2003> for the
-- definitive reference of the algorithm.

module Numeric.MCMC.Slice (
    mcmc
  , slice

  -- * Re-exported
  , MWC.create
  , MWC.createSystemRandom
  , MWC.withSystemRandom
  , MWC.asGenIO
  ) where

import Control.Monad.Trans.State.Strict (put, get, execStateT)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Lens hiding (index)
import GHC.Prim (RealWorld)
import Data.Maybe (fromMaybe)
import Data.Sampling.Types
import Pipes hiding (next)
import qualified Pipes.Prelude as Pipes
import System.Random.MWC.Probability (Prob, Gen)
import qualified System.Random.MWC.Probability as MWC

-- | Trace 'n' iterations of a Markov chain and stream them to stdout.
--
-- >>> let rosenbrock [x0, x1] = negate (5  *(x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)
-- >>> withSystemRandom . asGenIO $ mcmc 3 1 [0, 0] rosenbrock
-- -3.854097694213343e-2,0.16688601288358407
-- -9.310661272172682e-2,0.2562387977415508
-- -0.48500122500661846,0.46245400501919076
mcmc
  :: (Show (t a), FoldableWithIndex (Index (t a)) t, Ixed (t a),
     IxValue (t a) ~ Double)
  => Int
  -> Double
  -> t a
  -> (t a -> Double)
  -> Gen RealWorld
  -> IO ()
mcmc n radial chainPosition target gen = runEffect $
        chain radial Chain {..} gen
    >-> Pipes.take n
    >-> Pipes.mapM_ print
  where
    chainScore    = lTarget chainTarget chainPosition
    chainTunables = Nothing
    chainTarget   = Target target Nothing

-- A Markov chain driven by the slice transition operator.
chain
  :: (PrimMonad m, FoldableWithIndex (Index (t a)) t, Ixed (t a),
     IxValue (t a) ~ Double)
  => Double
  -> Chain (t a) b
  -> Gen (PrimState m)
  -> Producer (Chain (t a) b) m ()
chain radial = loop where
  loop state prng = do
    next <- lift (MWC.sample (execStateT (slice radial) state) prng)
    yield next
    loop next prng

-- | A slice sampling transition operator.
slice
  :: (PrimMonad m, FoldableWithIndex (Index (t a)) t, Ixed (t a),
      IxValue (t a) ~ Double)
  => Double
  -> Transition m (Chain (t a) b)
slice step = do
  Chain _ _ position _ <- get
  ifor_ position $ \index _ -> do
    Chain {..} <- get
    let bounds = (0, exp (lTarget chainTarget chainPosition))
    height    <- lift (fmap log (MWC.uniformR bounds))

    let bracket =
          findBracket (lTarget chainTarget) index step height chainPosition

    perturbed <- lift $
      rejection (lTarget chainTarget) index bracket height chainPosition

    let perturbedScore = lTarget chainTarget perturbed
    put (Chain chainTarget perturbedScore perturbed chainTunables)

-- Find a bracket by expanding its bounds through powers of 2.
findBracket
  :: (Ord a, Ixed s, IxValue s ~ Double)
  => (s -> a)
  -> Index s
  -> Double
  -> a
  -> s
  -> (IxValue s, IxValue s)
findBracket target index step height xs = go step xs xs where
  err = error "findBracket: invalid index -- please report this as a bug!"
  go !e !bl !br
    | target bl < height && target br < height =
        let l = fromMaybe err (bl ^? ix index)
            r = fromMaybe err (br ^? ix index)
        in  (l, r)
    | target bl < height && target br >= height =
        let br0 = expandBracketRight index step br
        in  go (2 * e) bl br0
    | target bl >= height && target br < height =
        let bl0 = expandBracketLeft index step bl
        in  go (2 * e) bl0 br
    | otherwise =
        let bl0 = expandBracketLeft index step bl
            br0 = expandBracketRight index step br
        in  go (2 * e) bl0 br0

expandBracketLeft
  :: (Ixed s, IxValue s ~ Double)
  => Index s
  -> Double
  -> s
  -> s
expandBracketLeft = expandBracketBy (-)

expandBracketRight
  :: (Ixed s, IxValue s ~ Double)
  => Index s
  -> Double
  -> s
  -> s
expandBracketRight = expandBracketBy (+)

expandBracketBy
  :: Ixed s
  => (IxValue s -> Double -> IxValue s)
  -> Index s
  -> Double
  -> s
  -> s
expandBracketBy f index step xs = xs & ix index %~ (`f` step )

-- Perform rejection sampling within the supplied bracket.
rejection
  :: (Ord a, PrimMonad m, Ixed b, IxValue b ~ Double)
  => (b -> a)
  -> Index b
  -> (Double, Double)
  -> a
  -> b
  -> Prob m b
rejection target dimension bracket height = go where
  go zs = do
    u <- MWC.uniformR bracket
    let  updated = zs & ix dimension .~ u
    if   target updated < height
    then go updated
    else return updated

