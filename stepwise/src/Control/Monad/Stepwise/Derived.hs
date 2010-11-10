-- | This module contains some utility functions that build on the
--   core interface of 'Stepwise' computations.
--
-- Todo: nicer abstractions for specific merge-patterns.
{-# LANGUAGE BangPatterns, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Control.Monad.Stepwise.Derived
  ( localChoice, mergeSteps              -- chooses the succeeding computation with the least progress reports
  ) where

import Control.Monad.Stepwise.Core

import Control.Applicative
import Control.Monad.Error
import Data.Monoid


-- | MonadError instance.
-- A 'throwError' without a 'catchError' is semantically equal to bottom.
-- 'catchError' runs the computation stepwise, until it succeeds (then drops
-- the handler), or fails (then runs the handler instead). However, if the
-- evaluation requires a continuation, we drop the handler, since we do not
-- know what the future is when the handler is present.
instance Error e => MonadError e (Stepwise e i o w) where
  throwError = abort
  catchError m h = case smallStep m of
    Step i m' -> info i (catchError m' h)
    Fin v     -> return v
    Failed e  -> h e
    Suspend f -> lookahead f

-- Applicative instance of 'Stepwise' computations.
instance Error e => Applicative (Stepwise e i o w) where
  pure    = return
  p <*> q = let !r1 = (\f -> let !r2 = return . f in q >>= r2)
            in p >>= r1

-- | Alternative instance.
--   Takes the shortest sequence that yields a
--   value, or the longest that fails.
instance (Monoid (i w), Error e, Monoid e) => Alternative (Stepwise e i o w) where
  empty   = fail "empty alternative"
  p <|> q = localChoice mergeSteps (\e1 e2 -> throwError (e1 `mappend` e2)) p q

-- | Merges two steps into a single step, thereby making use of the monoid instance.
mergeSteps :: (Monoid (i w), Error e, Monoid e) => i w -> Stepwise e i o w a -> i w -> Stepwise e i o w a -> Stepwise e i o w a
mergeSteps i1 p1 i2 p2 = info (i1 `mappend` i2) (p1 <|> p2)

-- | Chooses locally: i.e. does not allow a lookahead beyond the current computation. A subcomputation does not
-- see beyond the current choice.
localChoice :: Error e => (i w -> Stepwise e i o w a -> i w -> Stepwise e i o w a -> Stepwise e i o w a) -> (e -> e -> Stepwise e i o w a) -> Stepwise e i o w a -> Stepwise e i o w a -> Stepwise e i o w a
localChoice !f !g p q = merge (localStep p) (localStep q) where
  merge (Step i1 p1) (Step i2 p2) = f i1 p2 i2 p2
  merge (Fin x) _ = return x
  merge _ (Fin x) = return x
  merge (Failed s1) (Failed s2) = g s1 s2
  merge (Step i m) (Failed _)   = info i m
  merge (Failed _) (Step i m)   = info i m
