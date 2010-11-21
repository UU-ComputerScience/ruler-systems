{-# LANGUAGE GADTs, RankNTypes, BangPatterns, EmptyDataDecls, FlexibleInstances, DeriveDataTypeable #-}
module Control.Monad.Stepwise.Core
  ( Stepwise, StepHandle                 -- the type of a computation in the breadth-first monad
  , lazyEval, seqEval, stepwiseEval      -- evaluation of the result of a breadth-first computation
  , info, emit                           -- prefix/inject progress reports
  , localStep, smallStep, Progress(..)   -- step through step-wise computations
  , lookahead                            -- obtain the continuation
  , transcode, Transcoder(..)            -- transcode progress reports
  , translate, unsafeTranslate           -- transcoder variations
  , abort, final, failure, resume        -- primary combinators
  , lazily, sequentially                 -- evaluation options
  , share                                -- share a step-wise computation
  , task, nextTask                       -- the source of a progress report
  , handle, report, perform              -- monadic stepping operations
  , proceed, close                       -- embed handles
  , Report(..)                           -- data-type for progress reports
  , Sequential, Lazy                     -- type indices for the type of evaluation
  , AnyWatcher, AnyFailure               -- types representing "don't care about these aspects"
  , forceSequential                      -- forces the type to sequential
  , memoSteps, newMemoEnv, MemoEnvRef    -- memoize stepwise computations
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Error
import Control.Monad.Fix
import Control.Monad.Stepwise.Unsafe
import Control.Monad.Trans
import Data.IntMap(IntMap)
import Data.Maybe
import qualified Data.IntMap as IntMap
import Data.IORef
import Data.Monoid
import Data.Typeable
import GHC.Exts(inline,lazy)
import System.IO


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
--   however, apply 'share' to a 'Stepwise' value.
--
--   The additional indirection allows us to have explicit sharing,
--   such that we can update thunks, which opens up ways for parallelism.
--
--   The 'Mode' constructor serves three purposes. With it we can represent
--   entering a certain evaluation mode, leaving a certain evaluation mode,
--   and remembering the stack of evaluation modes we are currently in.

data Stepwise e i o w a where
  Pending :: !(Parents e i o v w b a) -> Stepwise e i o v b -> Stepwise e i o w a
  Ahead   :: !(forall b v . (forall o' . a -> Stepwise e i o' v b) -> Stepwise e i Lazy v b) -> Stepwise e i o w a
  Final   :: !a -> Stepwise e i o w a
  Info    :: !(i w) -> Stepwise e i o w a -> Stepwise e i o w a
  Fail    :: !e -> Stepwise e i o w a
  Ind     :: {-# UNPACK #-} !(StepRef e i o w a) -> Stepwise e i o w a
  Unpure  :: !(IO a) -> Stepwise e i o w a          -- warning: properly ordered I/O only in sequential evaluation mode
  Mode    :: !(Env -> Env) -> Stepwise e i o w a -> Stepwise e i o w a    -- mutates (or restores) evaluation environment

-- {-# RULES  "Pending/Root"   forall r . Pending Root r = r  #-}
-- {-# RULES  "Bind/Final"     forall c t . Cont (Bind Final) TransNone t c = c  #-}

-- | A handle to a Step-wise computation. If you use a handle to
--   request the next progress report, the effort to produce this report
--   is only performed once. If the handle is shared, it may not cost
--   any effort to get the next progress report.
newtype StepRef e i o w a = Ref (MVar (StepCell e i o w a))

-- | Represents a memory cell containing a stepwise computation.
--   Essential here is that we keep track of a partially reduced
--   computation, and possibly seperately, the outcome of a
--   lazy evaluation on that computation. Lazy evalutation reduces a computation
--   differently, outside of our control. We cannot simply replace the
--   computation with a 'Final v' value; only when strict evaluation of 'v'
--   is non-bottom - something we do not know. The reason is that the outcome of
--   a 'lazyEval m' is potentially less defined that the outcome 'stepwiseEval m'.
--   However, we do want to share multiple 'lazyEval's on the same computation,
--   hence a separate cache for stepwise and lazy evaluations.
data StepCell e i o w a
  = Cell  !(Stepwise e i o w a)          -- (partially) reduced computation
          !(Maybe a)                     -- potential cache of |lazyEval| on the computation

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
          !(Transcode e i v w) -> !(Parents e i o u w c a) ->
          Parents e i o v w b a
  Root :: Parents e i o w w a a

-- | Explicit closure for a pending bind.
data Operation e i o v w b a where
  Bind :: !(b -> Stepwise e i o w a) -> Operation e i o w w b a
  Code :: !(Transcode e i v w) -> Operation e i o v w a a


-- | Optional transcoding of info-messages. We expect not to transcode
--   often. Hence, we make explicit when no transcoding is done, such that
--   we can optimize the composition of transcoders.
--   Transcoding can in principle be by pattern matching on info-messages
--   and outputting them again. However, that would require a traversal
--   of the parent stack for each progress report. Instead, expecting that
--   there will only be a few transcoders, we compose these as a single
--   transcoder that immediate encodes to the target domain.
data Transcode e i v w where
  TransNone  :: Transcode e i w w
  TransCoder :: {-# UNPACK #-} !(Transcoder e i v w) -> Transcode e i v w


-- | Input to a transcoder.
--   'TcReport' represents a single report to be transcoded.
--   'TcDone' indicates that the computation to where this transcoder
--   is applied, has succeeded. 'TcFail' is its counter-part.
--   'TcLazy' indicates that a lazy evaluation has taken over the
--   computation.
data CodeIn e i w where
  TcReport :: !(i w) -> CodeIn e i w
  TcLazy   :: CodeIn e i w
  TcDone   :: CodeIn e i w
  TcFail   :: !e -> CodeIn e i w

-- | Output of a transcoder.
--   Either it succeeds with zero or more transcoded progress reports,
--   or it aborts the computation.
data CodeOut e i w where
  TcReports :: [i w] -> !(Maybe e) -> CodeOut e i w

-- | A transcoder is a function that transcodes a progress report of the
--   type @i v@ to reports of the type @i w@. It gets a 'CodeIn' as input
--   and produces a 'CodeOut' as output. The intention is that transcoders
--   are pure functions: side effect is allowed, but it is up to the
--   programmer to ensure that the progress report are not affected.
--   If the input is 'TcLazy', the transcoder is notified that lazy
--   evaluation starts running the computation. The outcome of the
--   transcoder is ignored. When this takes place is unspecified.
newtype Transcoder e i v w = Trans ((CodeIn e i v) -> IO (CodeOut e i w))


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
data AnyWatcher deriving Typeable

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
  mfix f = fixit where
    fixit = let r = Ind $ inlinePerformIO $ createRef $ f $ lazyEval r in r
    {-# NOINLINE fixit #-}

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
lookupRef :: StepRef e i o w a -> IO (StepCell e i o w a)
lookupRef (Ref h) = readMVar h

-- | Writes a reference to a stepwise computation, and releases it.
--   Note: a 'putRef' must follow exactly a 'takeRef'.
putRef :: StepRef e i o w a -> StepCell e i o w a -> IO ()
putRef (Ref h) !m = putMVar h m

-- | Takes the stepwise computation from the reference and reserves it.
--   Note: a 'takeRef' must be followed exactly by a 'putRef'.
takeRef :: StepRef e i o w a -> IO (StepCell e i o w a)
takeRef (Ref h) = takeMVar h

-- | Creates a reference to a stepwise computation.
--   Should not be strict in its second parameter!
--   The first argument is intended an optional reference to an earlier
--   version of the second parameter.
createRef :: Stepwise e i o w a -> IO (StepRef e i o w a)
createRef m = newMVar (Cell m Nothing) >>= return . Ref


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
eval env  (Ind h)       = inlinePerformIO $ do
                            c@(Cell m mbFin) <- takeRef h
                            case mbFin of  -- is Just at least when m is Final
                              Just v -> do putRef h c
                                           return v
                              _ -> let v = eval DefaultMode m   -- indirections are always evaluated in default context
                                   in do putRef h $ Cell m $ Just v  -- update the reference before forcing 'v'
                                         return $! v                 -- force the actual evaluation of 'v'
eval _    (Final v)     = v
eval env  (Ahead f)     = eval DefaultMode (f final)   -- there is no concept of choice in lazy mode, hence no lookahead
eval _    (Fail _)      = error "evaluation failed"
eval _    (Unpure m)    = let run = inlinePerformIO m
                              {-# NOINLINE run #-}
                          in run
eval env  (Mode f r)    = eval (f env) r

-- | Applies the pending binds.
evalStack :: ModeStack -> Parents e i o v w b a -> b -> a
evalStack !_  Root = id
evalStack env (Cont (Bind f) _ stack) = evalStack env stack . eval env . runInMode (peekEvalMode env) f
  where runInMode AllowLazy       = ($)
        runInMode ForceSequential = ($!)
evalStack env (Cont (Code t) _ stack) =
  let run = inlinePerformIO (runCoder t TcLazy)  -- tells transcoder that 'eval' takes over.
      {-# NOINLINE run #-}
  in run `seq` evalStack env stack


-- | A progress report. Either the progress report denotes a single
--   step, or a finished/failed computation, or a suspended computation
--   that waits for its future continuation before it can proceed.
data Progress e i o w a where
  Step      :: !(i w) -> Stepwise e i o w a -> Progress e i o w a
  Fin       :: !a -> Progress e i o w a
  Lookahead :: !(forall b v . (forall o' . a -> Stepwise e i o' v b) -> Stepwise e i Lazy v b) -> Progress e i o w a
  Failed    :: !e -> Progress e i o w a


-- | One step strict evaluation. Reduction proceeds until one
-- progress report entry is produced, or the computation is
-- suspended waiting for the continuation.
next :: Stepwise e i o w a -> IO (Progress e i o w a)
next r = do st <- newIORef DefaultMode
            trace Toplevel "next: obtained the next progress report."
            fst <$> nextReport st r

{-# NOINLINE next #-}

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
  writeIORef ref st0     -- restores the evaluation mode to before running the progress report
  trace Toplevel "nextReport: obtained the next proress report."
  return (remember st0 st1 p, st1)  -- returns r's mode stack as well, for potential continuations

-- Handles stepwise computation for the trivial cases, and
-- delegates the handling of 'Pending'-nodes to 'nextPending'.
next' :: EnvRef -> Stepwise e i o w a -> IO (Progress e i o w a)
next' !_  (Info i r) = return $ Step i r
next' env p@(Pending _ _) = inline nextPending env p
next' _   (Ind h)    = nextHandle h
next' _   (Final v)  = return $ Fin v
next' _   (Ahead f)  = return $ Lookahead f
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
--   If the value that comes out is an 'Info'-value, we again turn the remaining
--   computation into an indirection, in order to allow sharing of this remaining
--   computation.
nextHandle :: StepRef e i o w a -> IO (Progress e i o w a)
nextHandle !h = do
  c@(Cell m mbFin) <- takeRef h
  trace Notice "nextHandle: obtaining next progress report."
  p <- next m
  case p of   -- don't update a 'Failed' computation: lazyEval on it may still succeed
    Failed _ | isNothing mbFin -> putRef h c >> return p  
    _ -> do p' <- case p of  -- create an indirection to the remainder of the computation
                    Step i m1 -> createRef m1 >>= return . Step i . Ind
                    _         -> return p
            updateHandle h mbFin m (task p')
            return p'

-- | Conditionally updates the handle with the new stepwise computation, if the old computation is not trivial.
--  If the handle the computation points to is by itself an indirection, we keep the indirection: only tripple or
--  more indirections can be short-circuited without loosing sharing in some cases.
updateHandle :: StepRef e i o w a -> Maybe a -> Stepwise e i o w a -> Stepwise e i o w a -> IO ()
updateHandle h !mbFin (Pending _ _) cur = putRef h (Cell cur mbFin)   -- we reduced a step further
updateHandle h mbFin  (Unpure _)    cur = putRef h (Cell cur mbFin)   -- (may help to) prevent reexecution of IO
updateHandle h mbFin  m             _   = putRef h (Cell m mbFin)     -- update of handle insufficient gains


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
nextPending env  (Pending stack@(Cont o tr c) r) = 
  let (m, mb) = modeElim r
      mode Nothing  x = x
      mode (Just _) m@(Mode _ _) = m
      mode (Just f) r = Mode f r
  in case m of
       Pending stack1 r' -> do env' <- readIORef env
                               let !stack0 = case mb of  -- restores the old env after stack1
                                               Nothing -> stack  -- no need to restore stack if there is no mode change
                                               Just f  -> pushOper (\x -> Mode (const env') $! final x) stack
                               trace Notice "nextPending: merging stacks."
                               nextPending env (Pending (pushStack stack1 stack0) (mode mb r'))
       _ -> do (p, env') <- nextReport env (mode mb m)
               trace Notice "nextPending: reducing active node."
               case p of
                 Step i r' -> applyTranscoder env tr (TcReport i) (Pending stack r') >>= next' env
                 Failed e  -> applyTranscoder env tr (TcFail e) (Fail e) >>= next' env
                 _ -> case o of
                   Code t -> do
                     let !m' = case p of
                                 Fin v       -> Final v
                                 Lookahead k -> Ahead k
                     applyTranscoder env t TcDone m' >>= next' env . Pending c
                   Bind f  -> case p of
                     Fin v       -> next' env $ Pending c $ f v
                     Lookahead k -> next $ lazily $ k $! coerceMode . Mode (const env') . Pending c . f

-- | Applies the transcoder and builds a new computation, by prepending the transcoded steps,
--   and possibly replacing the initial computation with a failing one. This happens only when the
--   transcoder causes an abort.
applyTranscoder :: EnvRef -> Transcode e i v w -> CodeIn e i v -> Stepwise e i o w a -> IO (Stepwise e i o w a)
applyTranscoder !env !tr !tcIn !m = do
  tcOut <- runCoder tr tcIn
  case tcOut of
    TcReports rs mb ->
      let !m1 = maybe m Fail mb
          !m2 = foldr (\i' !k -> Info i' k) m1 rs
      in return m2

-- | Remembers the evaluation mode for the continuation.
restoreMode :: Env -> (b -> Stepwise e i o w a) -> b -> Stepwise e i o w a
restoreMode DefaultMode = id
restoreMode e           = \f -> Mode (const e) . f

-- | Removes 'Mode' constructors. The removed data can be reconstructed again
--   via inspection of the right argument
modeElim :: Stepwise e i o w a -> (Stepwise e i o w a, Maybe (Env -> Env))
modeElim (Mode f r) = 
  let !(r', mb) = modeElim r
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
      !t' = composeCoders t st
  in Cont (Code t) t' st
pushStack (Cont (Bind f) _ r) r' =
  let !st = pushStack r r'
      !t' = composeCoders TransNone st
  in Cont (Bind f) t' st


-- | Applies the transcoder. It may drop progress reports by returning
--   'Nothing'. It is not possible to apply changes to the remaining
--   computation. Transcoders are only applied during step-wise evaluation,
--   which would affect the outcome in case of lazy evaluation.
--   Todo: it may be interesting to yield additional progress reports.
runCoder :: Transcode e i v w -> CodeIn e i v -> IO (CodeOut e i w)
runCoder TransNone              !i = emptyCoder i
runCoder (TransCoder (Trans t))  i = t i

-- | Empty transcoder: just passes along what it gets.
emptyCoder :: CodeIn e i w -> IO (CodeOut e i w)
emptyCoder (TcReport i) = return (TcReports [i] Nothing)
emptyCoder (TcFail e)   = return (TcReports [] (Just e))
emptyCoder TcLazy       = return (TcReports [] Nothing)
emptyCoder TcDone       = return (TcReports [] Nothing)

-- | Pushes a transcoder on the parent's stack.
--   Note: a pending-node specifies how to transcode the progress reports
--         resulting from the operation. The transcoder that we
--         push on the stack translates progress reports of the active
--         node into the domain of the parents.
pushCoder :: Transcode e i v u -> Parents e i o u w a b -> Parents e i o v w a b
pushCoder TransNone c = c     -- keep the stack the same if we're not pushing any transcoder
pushCoder t c = Cont (Code t) (composeCoders t c) c

-- | Composes (efficiently) a global transcoder out of a local transcoder and
--   the global transcoder of the pending parent.
composeCoders :: Transcode e i v u -> Parents e i o u w a b -> Transcode e i v w
composeCoders !t Root = t
composeCoders t1 (Cont _ t2 _) = composeCoder t1 t2

-- | Composes two coders by applying the latter to the outcome of the former.
composeCoder :: Transcode e i v u -> Transcode e i u w -> Transcode e i v w
composeCoder TransNone t = t
composeCoder t TransNone = t
composeCoder (TransCoder (Trans f)) (TransCoder (Trans g)) = TransCoder (Trans h) where
  h inp = do
    out <- f inp
    case out of
      TcReports reps mbE -> do
        outs <- mapM (g . TcReport) reps
        let combine (TcReports rs1 mb1) (TcReports rs2 mb2) = TcReports (rs1 ++ rs2) (maybe mb2 Just mb1)
        return $! foldr combine (TcReports [] mbE) outs

-- | Pushes an operation on the stack.
pushOper :: (a -> Stepwise e i o u b) -> Parents e i o u w b c -> Parents e i o u w a c
pushOper f stack = Cont (Bind f) (composeCoders TransNone stack) stack

{-# INLINE runCoder #-}
{-# INLINE pushCoder #-}
{-# INLINE composeCoder #-}


-- gives the possibility to look at progress after the evaluation of the
-- current node via a 'Lookahead'.
smallStep :: Stepwise e i o w a -> Progress e i o w a
smallStep x = inlinePerformIO (next x)

-- only evaluate locally: progress of only the current node is
-- taken into account. It never returns a Lookahead result
localStep :: Stepwise e i o w a -> Progress e i o w a
localStep r = case smallStep r of
  Lookahead k -> localStep $ lazily $ k final
  p           -> p

-- | Evaluates step-wise (also ties the look-ahead knot)
stepwiseEval :: Stepwise e i o w a -> a
stepwiseEval r = case smallStep r of
  Step i r'   -> stepwiseEval r'
  Fin v       -> v
  Failed e    -> error "evaluation failed"
  Lookahead f -> stepwiseEval (f final)


-- | Applies a transcoder to a computation.
transcode :: Transcoder e i v w -> Stepwise e i o v a -> Stepwise e i o w a
transcode !f = Pending (pushCoder (TransCoder f) Root)

-- | Translates progress reports from one domain directly into another.
translate :: (i v -> i w) -> Stepwise e i o v a -> Stepwise e i o w a
translate !f = transcode $ Trans tr where
  tr (TcReport i) = return $ TcReports [f i] Nothing
  tr (TcFail e)   = return $ TcReports [] (Just e)
  tr TcDone       = return $ TcReports [] Nothing
  tr TcLazy       = return $ TcReports [] Nothing

-- | Assumes that 'i v' is structurally equal to 'i w'.
unsafeTranslate :: Stepwise e i o v a -> Stepwise e i o w a
unsafeTranslate = unsafeCoerce

{-# INLINE translate #-}
{-# INLINE transcode #-}
{-# INLINE unsafeTranslate #-}


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

-- | Helper function that demands that the type of the stepwise computation
--   is sequential.
forceSequential :: Stepwise e i Sequential w a -> Stepwise e i Sequential w a
forceSequential = id

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
{-# INLINE coerceMode #-}


-- | Shares a stepwise computation. Work for such a shared computation is only
--   performed once.
share :: Stepwise e i o v a -> Stepwise e i Sequential w (Stepwise e i o v a)
share m = unsafeIO (createRef m >>= return . Ind)


-- | Converts a progress report back into a thunk that upon
--   'next'-reduction immediately yields the progress report again.
task :: Progress e i o w a -> Stepwise e i o w a
task (Step i m)    = Info i m
task (Fin v)       = Final v
task (Failed s)    = Fail s
task (Lookahead k) = Ahead k

-- | Similar to 'task', except that it takes the next task of a step instead.
nextTask :: Progress e i o w a -> Stepwise e i o w a
nextTask (Step i m) = m
nextTask p = task p

{-# INLINE task #-}
{-# INLINE nextTask #-}


-- Handle-based access to stepwise computations as iterator. It keeps track of the latest report,
-- and you can advance it to the next report.
newtype StepHandle e i o w a = Handle (IORef (StepCursor e i o w a))

-- | A cursor that keeps track of the latest report and the latest computation.
data StepCursor e i o w a = Cursor (Stepwise e i o w a) !(Report e i o w a)

-- | The 'Report' version of a 'Progress' report.
--   The main difference is that this variation is handle-based, which provides
--   a monadic way of accessing the progress reports.
data Report e i o w a where
  Finished :: !a -> Report e i o w a
  Progress :: !(i w) -> Report e i o w a
  Failure  :: !e -> Report e i o w a
  Future   :: !(forall b v . (forall o' . a -> Stepwise e i o' v b) -> Stepwise e i Lazy v b) -> Report e i o w a
  Unavail  :: Report e i o w a
  
-- | Creates a handle to a stepwise computation.
handle :: Stepwise e i o v a -> Stepwise e i Sequential w (StepHandle e i o v a)
handle m = unsafeIO ((newIORef $! Cursor m Unavail) >>= return . Handle)

-- | Access the latest progress report on the handle.
report :: StepHandle e i o v a -> Stepwise e i Sequential w (Report e i o v a)
report (Handle h) = unsafeIO (readIORef h >>= \(Cursor _ r) -> return r)

-- | Progress the handle one step. Note that the handle maintains a reference to
--   the outcome of the previous computation. Hence, if this previous computation
--   was a 'Info', we need to continue with the computation as its rhs.
perform :: StepHandle e i o v a -> Stepwise e i Sequential w ()
perform (Handle h) = unsafeIO $ do
  Cursor m _ <- readIORef h
  p <- next m
  let (r,m') = case p of  -- map 'Progress' to both 'Report' and 'Stepwise'
        Step i m1   -> (Progress i, m1)  -- progress forwards
        Fin v       -> (Finished v, Final v)
        Failed e    -> (Failure e,  Fail e)
        Lookahead f -> (Future f,   Ahead f)
  writeIORef h $! Cursor m' r
  trace Toplevel "perform: performed a step for a handle."

-- | Closes the handle and returns the remaining computation. The remaining computation
--   emits the last progress report first (if any), because this report may not be
--   acted upon yet. If you don't want this behavior, apply a transcoder that filters
--   out the first report.
close :: StepHandle e i Lazy v a -> Stepwise e i Sequential w (Stepwise e i Lazy v a)
close (Handle h) = unsafeIO $ do
  Cursor m r <- readIORef h
  writeIORef h $! Cursor (error "handle is closed") Unavail
  case r of
    Progress i -> return (Info i m)
    _          -> return m

-- | Closes the handle and embeds the remaining computation.
proceed :: StepHandle e i Lazy w a -> Stepwise e i Sequential w a
proceed h = close h `resume` lazily

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


-- | Collects multiple error messages.
newtype Errors e = Errors [Either String e]

instance Monoid (Errors e) where
  mempty = Errors []
  e1 `mappend` e2 = Errors (toErrs e1 ++ toErrs e2)
    where toErrs (Errors xs) = xs

instance Error (Errors e) where
  noMsg    = mempty
  strMsg s = Errors [Left s]


-- | Turn tracing on or off
data TraceLevel = None | Toplevel | Notice

traceLevel :: TraceLevel
traceLevel = None

trace :: TraceLevel -> String -> IO ()
trace None !_ = return ()
trace Notice msg = case traceLevel of
  Toplevel -> printTraceMsg msg
  _         -> return ()
trace Toplevel msg = case traceLevel of
  None -> return ()
  _    -> printTraceMsg msg

printTraceMsg :: String -> IO ()
printTraceMsg msg
  = hPutStr stderr (msg ++ "\r\n")

{-# INLINE trace #-}


--
--  Memoizing stepwise computations
--

-- | Catched stepwise computations are existential in the return value.
data MemoEntry e i o w where
  Entry :: Typeable a => {-# UNPACK #-} !(StepRef e i o w a) -> MemoEntry e i o w

-- | The first indirection is the cache per return type, the second indirection the
--   values stored per key.
type MemoEnv e i o w = IntMap (IntMap (MemoEntry e i o w))

-- | Use a different 'MemoEnv' for different watcher types.
type MemoEnvRef e i o w = IORef (MemoEnv e i o w)

-- | Creates an empty memo-env.
newMemoEnv :: IO (MemoEnvRef e i o w)
newMemoEnv = newIORef IntMap.empty

-- | Memoizes a stepwise computation.
memoSteps :: Typeable a => MemoEnvRef e i o w -> Int -> Stepwise e i o w a -> Stepwise e i o w a 
memoSteps !ref !key val = Ind $! inlinePerformIO $ do
  mpTp  <- readIORef ref
  tpKey <- memoKey val
  case IntMap.lookup tpKey mpTp of
    Just mpKeys -> case IntMap.lookup key mpKeys of
      Nothing        -> insert tpKey
      Just (Entry h) -> return $! unsafeCoerce h
    Nothing     -> insert tpKey
  where
    {-# NOINLINE insert #-}
    insert tpKey = do
      h <- createRef val  -- does not force 'val' yet
      let upd mp = let mp1 = IntMap.findWithDefault IntMap.empty tpKey mp
                       mp2 = IntMap.insertWith (flip const) key (Entry h) mp1
                       mp' = IntMap.insert tpKey mp2 mp
                       h'  = case IntMap.lookup key mp2 of Just (Entry x) -> unsafeCoerce x
                   in (mp', h')
      h' <- atomicModifyIORef ref upd
      seq val (return h')  -- force 'val' to prevent storing closures in the memo-table.

-- | Produces a unique key based on the type 'a'
memoKey :: Typeable a => Stepwise e i o w a -> IO Int
memoKey m = typeRepKey $ typeOf $ extract m
  where
    extract :: Stepwise e i o w a -> a
    extract _ = error "memoKey: value should not be referred"

{-# INLINE memoKey #-}
{-# INLINE memoSteps #-}
{-# INLINE newMemoEnv #-}
