-- | Compatibility module. See Control.Monad.BreadthFirst
--
--   /The text of this module is out-dated/
--
--   These combinators form a domain-specific language for the breadth-first and lazy
--   evaluation of functions that incorporate backtracking.
--   A function (of intended type @a -> b@) has type @Fun i a b@ with these
--   combinators, with its actual representation being a @Closure i (a -> b)@,
--   a wrapped conventional Haskell function that takes as input a value
--   @Arg a@, and produces a computation @Comp i (a -> b)@ that after evaluation
--   produces a value @Res b@, and may yield progress reports of type @i@ during
--   this process. The functions themselves are thus expressed as conventional
--   Haskell functions; the computations are expressed via the combinators.
--
--   The type of @Closure@ is actually @Inh n -> Comp i n@, with the evaluation of
--   @Comp i n@ returning the value @Syn n@. For functions with more complicated
--   types than @n = a -> b@, type instances for @Syn n@ and @Inh n@ can be given
--   by the library user. The instances for @n = a -> b@ are @Arg a@ and @Res b@
--   respectively.
--
--   Type type @Comp i n@ represents the evaluation of the results of the function.
--   This evaluation can proceed in two ways: through the combinator @lazyEval@, we
--   immediately obtain a lazy @Syn n@ result. Alternatively, via combinators
--   @globalStep@ and @localStep@, we step through the evaluation. An evaluation
--   step consists of a progress report @i@, and an updated computation. Via the
--   combinator @inject@, we construct a computation that emits such a progress
--   report. The type @i@ is chosen by the library user, and must be the same for
--   the entire computation.
--
--   Via the combinator @final@, a value of type @Syn n@ is wrapped into a
--   a computation. Via combinator @resume@, we construct a computation that
--   depends on the outcome of another computation. Via combinator @invoke@,
--   a function (implemented with these combinators as @Closure i n@) is
--   applied to its inputs @Inh n@, resulting in its computation.
--
--   Multiple-computations CS can be merged into a single computation in various
--   ways: through step-wise evaluation CS can be evaluated in parallel, and a
--   new computation constructed by emiting some of the progress reports coming
--   out of these evaluations, with a final value based on the final values of
--   one or more of the CS computations. Alternatively, we can first step-wise
--   evaluate the CS computations to a certain point, then choose the remaining
--   computation of one of them as the resulting computation.
--
--   Decision-points can be encoded via merging. Such a merge point is introduced
--   via the merge combinator and passed as @Merger@ value. Such a merging procedure
--   gets a continuation of the computations that will be done after the evaluation
--   of the merge. This allows us to look ahead beyond the evaluation of the
--   current subtree. For instance, decisions taken at this location in the
--   tree may affect the future evaluation. When we apply a @globalStep@ on a
--   computation, there may be decision point requiring to look beyond the
--   decision we are currently making. In this case, it returns a @Suspend@,
--   which requires an approximation of the future evaluation.
--
--   The behavior of @resume@ depends on how the resulting progress reports
--   are interpreted. If these are interpreted lazily, the continuation
--   gets lazily the outcome of the computation it depends on. If these are
--   interpreted strictly, first the depended computation is evaluated
--   before proceeding with the continuation. Also, hybrid situations may
--   arise.

{-# LANGUAGE GADTs, TypeFamilies, EmptyDataDecls, RankNTypes, BangPatterns #-}
module Data.BreadthFirst.Cata
  ( Closure(Closure),Inh(Arg),Syn(Res),Fun
  , invoke,resume,final,merge,Merger(Merger)
  , module Control.Monad.BreadthFirst) where

import Control.Monad.BreadthFirst
import Control.Monad.Error


newtype Closure e i w n = Closure (Inh n -> BF e i w (Syn n))
data family Inh n :: *         -- index @n@ uniquely determines the type of the inherited and
data family Syn n :: *         -- synthesized attributes.

type Fun e i a b = Closure e i () (a -> b)
data instance Inh (a -> b) = Arg a
data instance Syn (a -> b) = Res b

-- | Unwraps a @Closure@
invoke :: Closure e i w n -> Inh n -> BF e i w (Syn n)
invoke (Closure f) inh = f inh

-- | Creates a computation depending on another computation.
resume :: Error e => BF e i w (Syn n) -> (Syn n -> BF e i w a) -> BF e i w a
resume m !k = m >>= k

final :: Error e => Syn n -> BF e i w (Syn n)
final = return

-- | Introduces a computation for merging child-progress reports while
--   taking also into account the effects that the merge has in the
--   evaluation of the parents. The remaining evaluation for the parents
--   is passed as continuation.

data Merger e i w n where
  Merger :: !(forall a v . (Syn n -> BF e i v a) -> BF e i v a) -> Merger e i w n

merge :: Merger e i w n -> BF e i w (Syn n)
merge (Merger f) = lookahead f
