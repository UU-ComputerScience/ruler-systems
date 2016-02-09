-- | This module contains some utility functions that build on the
--   core interface of 'Stepwise' computations.
--
-- Todo: nicer abstractions for specific merge-patterns.
{-# LANGUAGE BangPatterns, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RankNTypes #-}
module Control.Monad.Stepwise.Derived
  ( localChoice, mergeSteps              -- chooses the succeeding computation with the least progress reports
  , globalChoice
  ) where

import Control.Monad.Stepwise.Core

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State.Strict
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
    Step i m'   -> info i (catchError m' h)
    Fin v       -> return v
    Failed mb   -> h (maybe noMsg id mb)
    Lookahead f -> lookahead f

-- | Alternative instance.
--   Takes the shortest sequence that yields a
--   value, or the longest that fails.
instance (Monoid (i w), Monoid e, Error e) => Alternative (Stepwise e i o w) where
  empty   = fail "empty alternative"
  p <|> q = localChoice mergeSteps (\e1 e2 -> abort (e1 `mappend` e2)) p q

-- | Merges two steps into a single step, thereby making use of the monoid instance.
mergeSteps :: (Monoid (i w), Monoid e, Error e) => i w -> Stepwise e i o w a -> i w -> Stepwise e i o w a -> Stepwise e i o w a
mergeSteps i1 p1 i2 p2 = info (i1 `mappend` i2) (p1 <|> p2)

-- | Chooses locally: i.e. does not allow a lookahead beyond the current computation. A subcomputation does not
-- see beyond the current choice.
localChoice :: (i w -> Stepwise e i o w a -> i w -> Stepwise e i o w a -> Stepwise e i o w a) -> (e -> e -> Stepwise e i o w a) -> Stepwise e i o w a -> Stepwise e i o w a -> Stepwise e i o w a
localChoice !f !g p q = merge (localStep p) (localStep q) where
  merge (Step i1 p1) (Step i2 p2) = f i1 p1 i2 p2
  merge (Fin x) _ = final x
  merge _ (Fin x) = final x
  merge (Failed mb1) (Failed mb2) = case mb1 of
    Just s1 -> case mb2 of
      Just s2 -> g s1 s2
      Nothing -> failure mb1
    Nothing -> failure mb2
  merge (Step i m) (Failed _)   = info i m
  merge (Failed _) (Step i m)   = info i m


-- | Global choice.
--   Takes the computation with the shortest sequence of reports that succeeds, or the longest that fails.
--   First parameter is a transcoder that translates reports to the final domain.

globalChoice :: Error e => (forall v . Stepwise e i Lazy v a) -> (forall v . Stepwise e i Lazy v a) -> Stepwise e i o w a
globalChoice l r = lookahead (\k -> merge (l >>= k) (r >>= k))
  where merge = localChoice both (\e _ -> abort e)
        both i1 p1 _ p2 = info i1 (p1 `merge` p2)


-- | Customizeable merge function.
--   using events.

{-
newtype StepMerger g e i o w a = Merger (StateT (MergeState s g e i o w) (Stepwise e i o w) a)

data MergeState g e i o w = MergeState
  { handleLeft  :: MergeHandle g e i o w
  , handleRight :: MergeHandle g e i o w
  , actions     :: MergeActions g e i o w
  }

data MergeActions g e i o w = MergeActions
  {
  
  }
-}
