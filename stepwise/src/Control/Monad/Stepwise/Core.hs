{-# LANGUAGE GADTs, RankNTypes, BangPatterns, EmptyDataDecls, FlexibleInstances #-}
module Control.Monad.Stepwise.Core
  ( Stepwise, StepHandle                 -- the type of a computation in the breadth-first monad
  , lazyEval, seqEval, stepwiseEval      -- evaluation of the result of a breadth-first computation
  , info, emit                           -- prefix/inject progress reports
  , localStep, smallStep, Progress(..)   -- step through step-wise computations
  , lookahead                            -- obtain the continuation
  , transcode, Transcoder(..)            -- transcode progress reports
  , observe, unsafeObserve               -- transcoder variations
  , abort, final, failure, resume        -- primary combinators
  , lazily, sequentially                 -- evaluation options
  , task                                 -- the source of a progress report
  , handle, proceed, report, perform     -- monadic stepping operations
  , Report(..)                           -- data-type for progress reports
  , Sequential, Lazy                     -- type indices for the type of evaluation
  , AnyWatcher, AnyFailure               -- types representing "don't care about these aspects"
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Fix
import Control.Monad.Trans
import Data.IORef
import Data.Monoid
import GHC.Exts(inline,lazy)
import Control.Monad.Stepwise.Unsafe


-- | A step-wise computation with errors @e@, progress reports @i@,
--   parametrized by watcher @w@, and evaluating to a value of type
--   @a@.
--
--   Progress reports @i@ are indexed by the watcher type @w@. To
--   compose step-wise computations, they must agree on the same
--   type @i@. However, specific caller/callee combinations can
--   agree on a type @w@ to report progress reports that contain
--   e.g. values computing during the evaluation process.
--
--   A stepwise computation may fail with an error of type @e@.
--   Failure is conceptually just another form of progress reports:
--   however, after a failure, there will not be any subsequent
--   progress reports. This distinction allows us to capture the
--   behavior of the 'fail' function in the 'Monad' class.
--   For non-critical failures, use conventional progress reports.
--   If the information about the failure is not an issue, use
--   either @AnyFailure@ or @String@ as the type for @e@.
--
--   A stepwise computation specifies its operational context via
--   the type index @o@. There are two operational modes: either
--   the computation requires to be executed sequentially, then
--   it has 'Sequential' as type index, or it may be executed lazily,
--   then it has 'Lazy' as type index. Some operations on stepwise
--   computations may require evaluation to be sequential. There is
--   no way (neither need) to enforce lazy evaluation.
--
--   A 'Stepwise'-value represents a partially evaluated step-wise
--   computation.
--   It is essentially a sequence of 'Info' progress reports, closed
--   by either failure, success, or the remaining computation.
--
--   The 'Pending' constructor
--   specifies the computation that is 'left-most'. Strict
--   evaluation starts with this computation first. It also specifies
--   the stack of binds that immediately follow the left-most computation.
--   Since the computation to evaluate first is always on top of this
--   structure, we do not have to inspect the stack for each reduction
--   step.
--
--   The 'Ahead' constructor represents a
--   suspended computation that needs a continuation, such that it
--   can give the reports for the final result.
--   Note that the function of 'Ahead' takes a continuation that cannot
--   make any assumption about how it is executed (hence the universal @o'@).
--   If it needs to make an assumption, it should do so via e.g. 'lazily'.
--   Furthermore, the function itself makes the assumption that it is
--   executed in a lazy context. This is a design choice: we could also
--   have demanded that it cannot make any assumptions on how it is called.
--
--  'Info' represents a progress report.
--
--   The 'Ind' constructor represents an indirection.
--   Sharing an indirection has the effect that the effort of producing
--   the progress reports is only performed once. In
--   practice are 'Stepwise' values produced by functions, hence
--   sharing is not provided by default. To have a sharing guarantee,
--   however, obtain a 'StepHandle' to a 'Stepwise' value.
--
--   The additional indirection allows us to have explicit sharing,
--   such that we can update thunks, which opens up ways for parallelism.
--
--   The 'Mode' constructor serves three purposes. With it we can represent
--   entering a certain evaluation mode, leaving a certain evaluation mode,
--   and remembering the stack of evaluation modes we are currently in.

data Stepwise e i o w a where
  Pending :: !(Parents e i o v w b a) -> Stepwise e i o v b -> Stepwise e i o w a
  Ahead   :: (forall b v . (forall o' . a -> Stepwise e i o' v b) -> Stepwise e i Lazy v b) -> Stepwise e i o w a
  Final   :: !a -> Stepwise e i o w a
  Info    :: !(i w) -> Stepwise e i o w a -> Stepwise e i o w a
  Fail    :: !e -> Stepwise e i o w a
  Ind     :: {-# UNPACK #-} !(StepHandle e i o w a) -> Stepwise e i o w a
  Unpure  :: !(IO a) -> Stepwise e i o w a          -- warning: properly ordered I/O only in sequential evaluation mode
  Mode    :: !(Env -> Env) -> Stepwise e i o w a -> Stepwise e i o w a    -- mutates (or restores) evaluation environment

-- {-# RULES  "Pending/Root"   forall r . Pending Root r = r  #-}
-- {-# RULES  "Bind/Final"     forall c t . Cont (Bind Final) TransNone t c = c  #-}

-- | A reference to a Step-wise computation. If you use a handle to
--   request the next progress report, the effort to produce this report
--   is only performed once. If the handle is shared, it may not cost
--   any effort to get the next progress report.
newtype StepHandle o e i w a = Ref (IORef (Stepwise o e i w a))


-- | 'Parents' represents the stack of pending binds of a computation, as
--   well as transcoders for progress reports. It specifies how we can
--   transform the value @b@ to @a@, and transcode steps from @i v@ to @i w@.
--   The stack is structured in
--   such a way that we do not need to inspect the entire structure for
--   a single reduction step. For this purpose, we also store two
--   transcoders-values: a transcoder that only transcodes progress reports
--   to the format expected by its immediate parent, and automatically /derived/
--   from these a transcoder that transcodes directly to the format expected by
--   the root.
data Parents e i o v w b a where
  Cont :: {-# UNPACK #-} !(Operation e i o v u b c) ->
          !(Transcode i v w) -> !(Parents e i o u w c a) ->
          Parents e i o v w b a
  Root :: Parents e i o w w a a

-- | Explicit closure for a pending bind.
data Operation e i o v w b a where
  Bind :: !(b -> Stepwise e i o w a) -> Operation e i o w w b a
  Code :: !(Transcode i v w) -> Operation e i o v w a a


-- | Optional transcoding of info-messages. We expect not to transcode
--   often. Hence, we make explicit when no transcoding is done, such that
--   we can optimize the composition of transcoders.
--   Todo: we might as well transcode from i to some j as well.
data Transcode i v w where
  TransNone  :: Transcode i w w
  TransCoder :: {-# UNPACK #-} !(Transcoder i v w) -> Transcode i v w


-- | A transcoder maps a progress report of the type @i v@ to @i w@. If the
--   transcoder returns 'Nothing', the progress report is discarded.
newtype Transcoder i v w = Trans (i v -> Maybe (i w))


-- | Evaluation-mode stack. The stack keeps in what evaluation context
--   (possibly lazy or required sequential) the computation is.
data ModeStack
  = ExplicitMode  {-# UNPACK #-} !EvalMode  !ModeStack
  | DefaultMode

type Env = ModeStack
type EnvRef = IORef Env

-- | In the 'AllowLazy' mode, we may either evaluate lazily or stepwise.
--   In 'ForceSequential' mode, however, evaluation must be strict or
--   stepwise.
data EvalMode = ForceSequential | AllowLazy

-- | Type level version of 'AllowLazy'
data Lazy

-- | Type level version of 'ForceSequential'
data Sequential

-- | Type index representing an arbitrary watcher.
--   Note: in such situations, you can choose an arbitrary type. This type, however,
--   explicitly states that there is no interest in the watcher type, which provides
--   a bit additional documentation.
data AnyWatcher

-- | Type index representing arbitrary failure. No information is provided about the
--   failure - only that it happened. We provide instances to treat 'AnyFailure' as
--   error messages, which makes them convenient to use.
data AnyFailure = AnyFailure


-- | Pushes a new evaluation mode on top of the stack.
pushEvalMode :: EvalMode -> ModeStack -> ModeStack
pushEvalMode = ExplicitMode

-- | Pushes the entire stack of evaluation modes.
pushModeStack :: ModeStack -> ModeStack -> ModeStack
pushModeStack st DefaultMode = st
pushModeStack DefaultMode st = st
pushModeStack (ExplicitMode m st) st' = ExplicitMode m (pushModeStack st st')

-- | Pops the latest evaluation mode off the stack.
popEvalMode :: ModeStack -> ModeStack
popEvalMode (ExplicitMode _ st) = st
popEvalMode st = st

-- | Gets the current evaluation mode.
peekEvalMode :: ModeStack -> EvalMode
peekEvalMode (ExplicitMode m _) = m
peekEvalMode DefaultMode = AllowLazy


-- | Monad instance for Stepwise. See 'Control.Monad.BreadthFirst.Proofs' for
--   proofs of the monad laws.
instance Error e => Monad (Stepwise e i o w) where
  return = final
  fail   = failure . strMsg
  (>>=)  = resume

  {-# SPECIALIZE instance Error e => Monad (Stepwise e i o w) #-}
  {-# SPECIALIZE instance Monad (Stepwise (Errors e) i o w)   #-}
  {-# SPECIALIZE instance Monad (Stepwise String i o w)       #-}
  {-# SPECIALIZE instance Monad (Stepwise AnyFailure i o w)     #-}

instance Error e => Functor (Stepwise e i o w) where
  fmap f m = let !r = final . f in resume m r

-- | Instance for MonadFix.
--   Note: the steps resulting from @mfix f@ should not depend
--   on the actual outcome of @mfix f@: this would create a hard cycle.
instance Error e => MonadFix (Stepwise e i Lazy w) where
  mfix f = let r = Ind $ inlinePerformIO $ createRef $ f $ lazyEval r in r

-- | Instance for MonadIO.
--   The relative order of liftIO's, and non-duplication of effects, is only
--   guaranteed in a sequential context. Use with caution.
instance Error e => MonadIO (Stepwise e i Sequential w) where
  liftIO = unsafeIO

-- | In contrast to 'liftIO' does this function not require a
--   sequential evaluation context.
unsafeIO :: IO a -> Stepwise e i o w a
unsafeIO = Unpure

-- | Trivial instance for 'AnyFailure'.
instance Monoid AnyFailure where
  mempty = AnyFailure
  AnyFailure `mappend` AnyFailure = AnyFailure

-- | Turn error messages in AnyFailure, effectively loosing
--   all details (if any).
instance Error AnyFailure where
  noMsg    = AnyFailure
  strMsg _ = AnyFailure  -- does not store the message
  {-# SPECIALIZE instance Error AnyFailure #-}


-- | Obtains a (recent) thunk associated with the handle.
lookupRef :: StepHandle e i o w a -> Stepwise e i o w a
lookupRef m = inlinePerformIO $ lookupRefIO m

-- | Obtains a (recent) thunk associated with the handle.
lookupRefIO :: StepHandle e i o w a -> IO (Stepwise e i o w a)
lookupRefIO (Ref h) = readIORef h

-- | Updates a reference to a stepwise computation.
updateRef :: StepHandle e i o w a -> Stepwise e i o w a -> IO ()
updateRef (Ref h) !m = writeIORef h m

-- | Creates a reference to a stepwise computation.
--   Should not be strict in its argument!
createRef :: Stepwise e i o w a -> IO (StepHandle e i o w a)
createRef m = newIORef m >>= return . Ref


-- | Lazy evaluation of a step-wise computation.
lazyEval :: Stepwise e i Lazy w a -> a
lazyEval = eval DefaultMode

-- | Sequential evaluation of a step-wise computation.
seqEval :: Stepwise e i Sequential w a -> a
seqEval = eval (ExplicitMode ForceSequential DefaultMode)


-- | Evaluation of a step-wise computation.
--   We cannot return the progress reports, as this would
--   linearize the entire computation.
--
--   When using lazy evaluation, the left-hand side of a bind
--   is not guaranteed to be evaluated if the right-hand side is lazy in its
--   parameter. I.e. this behavior is similar as with the @Identity@ monad.
--
--   Invariant concerning the 'Env': it is only passed topdown. For 'eval',
--   we don't need to know when we leave a certain evaluation mode
eval :: Env -> Stepwise e i o w a -> a
eval !env (Info _ r)    = eval env r
eval env  (Pending stack r) = inline evalStack env stack $ eval env r
eval env  (Ind h)       = eval DefaultMode (lookupRef h)  -- indirections are always evaluated in default context
eval _    (Final v)     = v
eval env  (Ahead f)     = eval env (f final)
eval _    (Fail _)      = error "evaluation failed"
eval _    (Unpure m)    = inlinePerformIO m
eval env  (Mode f r)    = eval (f env) r

-- | Applies the pending binds.
evalStack :: ModeStack -> Parents e i o v w b a -> b -> a
evalStack !_  Root = id
evalStack env (Cont (Bind f) _ stack) = evalStack env stack . eval env . sequential (peekEvalMode env) f
  where sequential AllowLazy       = ($)
        sequential ForceSequential = ($!)
evalStack env (Cont (Code _) _ stack) = evalStack env stack


-- | A progress report. Either the progress report denotes a single
--   step, or a finished/failed computation, or a suspended computation
--   that waits for its future continuation before it can proceed.
data Progress e i o w a where
  Step    :: !(i w) -> Stepwise e i o w a -> Progress e i o w a
  Fin     :: !a -> Progress e i o w a
  Suspend :: !(forall b v . (forall o' . a -> Stepwise e i o' v b) -> Stepwise e i Lazy v b) -> Progress e i o w a
  Failed  :: !e -> Progress e i o w a


-- | One step strict evaluation. Reduction proceeds until one
-- progress report entry is produced, or the computation is
-- suspended waiting for the continuation.
next :: Stepwise e i o w a -> IO (Progress e i o w a)
next r = do st <- newIORef DefaultMode
            putStrLn "next"
            fst <$> nextReport st r

-- Assumes a start-evaluation mode of DefaultMode.
-- However, when it returns a 'Step' progress report,
-- it stores deviations to this mode into an additional
-- 'Mode' node. The next continuation thus restores
-- this state.
-- For stepwise evaluations, the evaluation mode does not matter. It always evaluates
-- sequentially. However, 'lazyEval' may be called onto a partially reduced computation.
-- We thus need to keep track of what context it is in, such that 'lazyEval' can decide
-- upon the right evaluation mode.
nextReport :: EnvRef -> Stepwise e i o w a -> IO (Progress e i o w a, Env)
nextReport ref r = do
  st0 <- readIORef ref
  p   <- next' ref r
  st1 <- readIORef ref
  writeIORef ref st0     -- restores the evaluation stores to before running the progress report
  putStrLn "nextReport"
  return (remember st0 st1 p, st1)  -- returns r's mode stack as well, for potential continuations

-- Handles stepwise computation for the trivial cases, and
-- delegates the handling of 'Pending'-nodes to 'nextPending'.
next' :: EnvRef -> Stepwise e i o w a -> IO (Progress e i o w a)
next' !_  (Info i r) = return $ Step i r
next' env p@(Pending _ _) = inline nextPending env p
next' _   (Ind h)    = nextHandle h
next' _   (Final v)  = return $ Fin v
next' _   (Ahead f)  = return $ Suspend f
next' _   (Fail e)   = return $ Failed e
next' _   (Unpure m) = Fin <$> m
next' env (Mode f r) = modifyIORef env f >> next' env r  -- applies the changes to the mode stack

-- | Keep track of the evaluation mode. The invariant is that 'next'
--   always starts in DefaultMode. The Stepwise computation should thus
--   make sure to remember deviations in the computation.
remember :: Env -> Env -> Progress e i o w a -> Progress e i o w a
remember st0 st1 p@(Step i r) = remember' st0 st1 where
  remember' DefaultMode DefaultMode = p                             -- no need for additional indirection
  remember' _           st          = Step i $! Mode (const st) r   -- completely replace stack
remember _ _ p = p                                                  -- no need to keep track for anything but steps


-- | Invariant: handles are always followed with a default operation environment, to
--   ensure that they can be reduced from different contexts. If the evaluation mode
--   differs from the default, the computation referred to should use a 'Mode'
--   instruction to setup this context.
--   We enforce that handles can only be created to 'Lazy' computations,
--   such that a default context works out.
nextHandle :: StepHandle e i o w a -> IO (Progress e i o w a)
nextHandle !h = do
  m <- lookupRefIO h
  p <- next m
  case p of                      -- determine if it is a good idea to update the handle
    Failed _ -> return ()        -- don't update a Failed computation: lazyEval on it may still succeed
    _        -> updateHandle h m (task p)
  return p

-- | Conditionally updates the handle with the new stepwise computation, if the old computation is not trivial.
updateHandle :: StepHandle e i o w a -> Stepwise e i o w a -> Stepwise e i o w a -> IO ()
updateHandle h (Pending _ _) cur = updateRef h cur   -- we reduced this partially evaluated result a bit further: update it
updateHandle h (Ind _ )      cur = updateRef h cur   -- shortcut double indirections
updateHandle h (Unpure _)    cur = updateRef h cur   -- (may help to) prevent reexecution of IO
updateHandle h _             _   = return ()


-- | Reduces a pending computation until it can yield a step.
--   when the computation finished, it proceeds with the evaluation
--   of the next parent on the stack.
--
--   The evaluation stack we take as parameter applies only to the whole
--   pending compution. The evaluations performed for the active compution
--   may influence this stack. We need to make sure that upon resume, that
--   computation gets its new stack again (via a 'Mode'-node), and also
--   restore our stack again.
nextPending :: EnvRef -> Stepwise e i o w a -> IO (Progress e i o w a)
nextPending !env (Pending Root r) = next' env r
nextPending env  (Pending stack@(Cont _ tr _) r) = 
  let (m, mb) = modeElim r
      mode Nothing  x = x
      mode (Just _) m@(Mode _ _) = m
      mode (Just f) r = Mode f r
  in case m of
       Pending stack1 r' -> do env' <- readIORef env
                               let !stack0 = case mb of  -- restores the old env after stack1
                                               Nothing -> stack  -- no need to restore stack if there is no mode change
                                               Just f  -> pushOper (\x -> Mode (const env') $! final x) stack
                               putStrLn "combining double stacks"
                               nextPending env (Pending (pushStack stack1 stack0) (mode mb r'))
       _ -> do (p, env') <- nextReport env (mode mb m)
               putStrLn ("nextPending")
               case p of
                 Step i r' -> case applyCoder tr i of
                                Nothing -> nextPending env $ Pending stack r'
                                Just i' -> return $ Step i' $! Pending stack r'
                 Failed s  -> return $ Failed s
                 Fin v     -> next' env $ reduceFinal stack env' v
                 Suspend k -> next $ lazily $ k $! coerceMode . reduceFinal stack env'

-- | Removes transcoders from the parents' stack until it encounters a continuation
--   to deliver the value to.
reduceFinal :: Parents e i o v w b a -> Env -> b -> Stepwise e i o w a
reduceFinal Root !_ = Final
reduceFinal (Cont (Code _) _ c) env = reduceFinal c env
reduceFinal (Cont (Bind f) _ c) env = Pending c . restoreMode env f

-- | Remembers the evaluation mode for the continuation.
restoreMode :: Env -> (b -> Stepwise e i o w a) -> b -> Stepwise e i o w a
restoreMode DefaultMode = id
restoreMode e           = \f -> Mode (const e) . f

-- | Removes 'Mode' constructors. The removed data can be reconstructed again
--   via inspection of the right argument
modeElim :: Stepwise e i o w a -> (Stepwise e i o w a, Maybe (Env -> Env))
modeElim (Mode f r) = let !(r', mb) = modeElim r
                          !mb' = case mb of
                                   Nothing -> Just $! f
                                   Just g  -> Just $! (g . f)
                      in (r', mb')
modeElim r = (r, Nothing)

-- | Pushes the first stack on top of the latter stack, by appending the latter
--   as tail of the former, and reconstructing the global transcoders. The idea
--   is that afterward, progress reports can be passed on efficiently, bypassing
--   all pending parents. Also the idea is that the stack we push onto this stack
--   is rather small - typically not more than one operation - because we push
--   the pending-stack to the place where evaluation happens.
pushStack :: Parents e i o v w c b -> Parents e i o w u b a -> Parents e i o v u c a
pushStack Root !r' = r'
pushStack (Cont (Code t) _ r) r' =
  let !st = pushStack r r'
      !t' = composeCoder t st
  in Cont (Code t) t' st
pushStack (Cont (Bind f) _ r) r' =
  let !st = pushStack r r'
      !t' = composeCoder TransNone st
  in Cont (Bind f) t' st

{-# INLINE next #-}


-- | Applies the transcoder. It may drop progress reports by returning
--   'Nothing'. It is not possible to apply changes to the remaining
--   computation. Transcoders are only applied during step-wise evaluation,
--   which would affect the outcome in case of lazy evaluation.
--   Todo: it may be interesting to yield additional progress reports.
applyCoder :: Transcode i v w -> i v -> Maybe (i w)
applyCoder TransNone              i = Just i
applyCoder (TransCoder (Trans t)) i = t i


-- | Pushes a transcoder on the parent's stack.
--   Note: a pending-node specifies how to transcode the progress reports
--         resulting from the operation. The transcoder that we
--         push on the stack translates progress reports of the active
--         node into the domain of the parents.
pushCoder :: Transcode i v u -> Parents e i o u w a b -> Parents e i o v w a b
pushCoder TransNone c = c     -- keep the stack the same if we're not pushing any transcoder
pushCoder t c = Cont (Code t) (composeCoder t c) c

-- | Composes (efficiently) a global transcoder out of a local transcoder and
--   the global transcoder of the pending parent.
composeCoder :: Transcode i v u -> Parents e i o u w a b -> Transcode i v w
composeCoder t Root = t
composeCoder TransNone (Cont _ t _) = t
composeCoder t (Cont _ TransNone _) = t
composeCoder (TransCoder (Trans f)) (Cont _ (TransCoder (Trans g)) _)
  = TransCoder $ Trans $ \i -> f i >>= g

-- | Pushes an operation on the stack.
pushOper :: (a -> Stepwise e i o u b) -> Parents e i o u w b c -> Parents e i o u w a c
pushOper f stack = Cont (Bind f) (composeCoder TransNone stack) stack


-- gives the possibility to look at progress after the evaluation of the
-- current node via a Suspend
smallStep :: Stepwise e i o w a -> Progress e i o w a
smallStep x = inlinePerformIO (next x)

-- only evaluate locally: progress of only the current node is
-- taken into account. It never returns a Suspend result
localStep :: Stepwise e i o w a -> Progress e i o w a
localStep r = case smallStep r of
  Suspend k -> localStep $ lazily $ k final
  p         -> p

-- | Evaluates step-wise (also ties the look-ahead knot)
stepwiseEval :: Stepwise e i o w a -> a
stepwiseEval r = case smallStep r of
  Step i r' -> stepwiseEval r'
  Fin v     -> v
  Failed e  -> error "evaluation failed"
  Suspend f -> stepwiseEval (f final)

{-# INLINE applyCoder #-}
{-# INLINE pushCoder #-}


transcode :: Transcoder i v w -> Stepwise e i o v a -> Stepwise e i o w a
transcode !f = Pending (pushCoder (TransCoder f) Root)

observe :: (i v -> i w) -> Stepwise e i o v a -> Stepwise e i o w a
observe !f = transcode $ Trans $ Just . f

unsafeObserve :: Stepwise e i o v a -> Stepwise e i o w a
unsafeObserve = unsafeCoerce

{-# INLINE observe #-}
{-# INLINE transcode #-}
{-# INLINE unsafeObserve #-}


-- | Abort a computation. Note that in lazy evaluation mode,
--   abort is semantically equivalent to bottom, whereas in
--   stepwise evaluation, it provides backtracking.
--   This means that if there is no backtracking-alternative
--   left, aborts are replaced by a bottom value.
abort :: e -> Stepwise e i o w a
abort =  Fail

-- | Turn a result into a (trivial) stepwise compuation.
final :: a -> Stepwise e i o w a
final = Final

-- | Creates an always failing stepwise computation.
failure :: e -> Stepwise e i o w a
failure = Fail

-- | Creates a pending computation for @m@ with @f@ on the stack of parents.
resume :: Stepwise e i o w b -> (b -> Stepwise e i o w a) -> Stepwise e i o w a
resume m f = Pending (Cont (Bind f) TransNone Root) m

{-# INLINE abort #-}
{-# INLINE final #-}
{-# INLINE resume #-}


-- | Allows the stepwise computation to run in lazy mode.
lazily :: Stepwise e i Lazy w a -> Stepwise e i o w a
lazily = withMode AllowLazy

-- | Forces the stepwise computation to run in sequential mode.
sequentially :: Stepwise e i Sequential w a -> Stepwise e i o w a
sequentially = withMode ForceSequential

-- | Runs the given stepwise computation in a certain evaluation mode.
--   It uses the 'Mode' constructor to indicate the transition to the
--   new evaluation mode. It also encodes with a 'Mode' the transition
--   back. This, however, is only needed for stepwise evaluation: the
--   scope of the lazy/sequential eval functions already determines
--   the scope of the evaluation mode.
withMode :: EvalMode -> Stepwise e i o' w a -> Stepwise e i o w a
withMode !m !r = Mode (pushEvalMode m) (resume (coerceMode r) $ \x -> Mode popEvalMode (final x)) where

-- | Coerces the evaluation context type to an arbitrary evaluation
--   context type. This is a phantom type, with the only purpose to
--   make sequential computations safer to use.
coerceMode :: Stepwise e i o' w a -> Stepwise e i o w a
coerceMode = unsafeCoerce -- assume structurally equal

{-# INLINE withMode #-}


-- | Converts a progress report back into a thunk that upon
--   'next'-reduction immediately yields the progress report again.
task :: Progress e i o w a -> Stepwise e i o w a
task (Step i m)  = Info i m
task (Fin v)     = Final v
task (Failed s)  = Fail s
task (Suspend k) = Ahead k


-- | The 'Report' version of a 'Progress' report.
--   The main difference is that this variation is handle-based, which provides
--   a monadic way of accessing the progress reports.
data Report e i o w a where
  Finished :: !a -> Report e i o w a
  Progress :: !(i w) -> Report e i o w a
  Failure  :: !e -> Report e i o w a
  Future   :: !(forall b v . (forall o' . a -> Stepwise e i o' v b) -> Stepwise e i Lazy v b) -> Report e i o w a
  
-- | Creates a handle to a stepwise computation.
handle :: Stepwise e i o' v a -> Stepwise e i Sequential w (StepHandle e i o' v a)
handle m = unsafeIO $ createRef m

-- | Access the latest progress report on the handle.
report :: StepHandle e i o' v a -> Stepwise e i Sequential w (Report e i o' v a)
report h = unsafeIO $ do
  m <- lookupRefIO h
  case m of
    Info i _  -> return $ Progress i
    Final v   -> return $ Finished v
    Fail s    -> return $ Failure s
    Ahead f   -> return $ Future f

-- | Progress the handle one step.
perform :: StepHandle e i o' v a -> Stepwise e i Sequential w ()
perform h = unsafeIO $ do
  m  <- lookupRefIO h
  p  <- next m
  updateRef h (task p)

-- | Turns a handle into a computation, thus proceeding with
--   the computation stored in the handle.
proceed :: StepHandle e i Lazy w a -> Stepwise e i Sequential w a
proceed = coerceMode . Ind

{-# INLINE proceed #-}


-- | Wrapper for an effect.
info :: i w -> Stepwise e i o w a -> Stepwise e i o w a
info i = Info i

emit :: i w -> Stepwise e i o w ()
emit i = info i (final ())

{-# INLINE info #-}
{-# INLINE emit #-}


-- | Introduces a computation for merging child-progress reports while
--   taking also into account the effects that the merge has in the
--   evaluation of the parents. The remaining evaluation for the parents
--   is passed as continuation.

lookahead :: (forall b v . (forall o' . a -> Stepwise e i o' v b) -> Stepwise e i Lazy v b) -> Stepwise e i o w a
lookahead = Ahead

{-# INLINE lookahead #-}


-- | Collects multiple error messages

newtype Errors e = Errors [Either String e]

instance Monoid (Errors e) where
  mempty = Errors []
  e1 `mappend` e2 = Errors (toErrs e1 ++ toErrs e2)
    where toErrs (Errors xs) = xs

instance Error (Errors e) where
  noMsg    = mempty
  strMsg s = Errors [Left s]
