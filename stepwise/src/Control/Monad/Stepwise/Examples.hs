-- | This module shows some example stepwise-computations, and
--   focus on individual features provided by the library. We
--   start with testing out some basic functionality, then
--   switch to more interesting examples.
--   In practice, you'll combine several of the features presented
--   here.

{-# LANGUAGE GADTs #-}
module Control.Monad.Stepwise.Examples where

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.State.Lazy
import Control.Monad.Stepwise.Core
import Control.Monad.Stepwise.Derived
import Control.Monad.Trans
import Data.Monoid
import Data.Set(Set)
import qualified Data.Set as Set


-- | A type for the simplest form of progress report: just a message 'I' that
--   indicates that a bit of work has been done. It is indexed by the watcher
--   type 't', which in this case doesn't matter. Later examples show a more
--   involving type of progress report that uses the watcher type.
data I t = I

-- | Monoid instance for use in combination with '<|>'. In practice, you'll not
--   use '<|>', but write more complicated merging strategies instead.
instance Monoid (I t) where
  mempty = I
  I `mappend` I = I


-- | Test 1: verify that results are provided when available (online behavior).
-- With 'lazyEval' this means that the result should be delivered, independent
-- of the failure. Failure is just considered to be a bottom-value: if it's never
-- needed in the continuation, it is not triggered. This is different in comparison
-- to strict evaluation.
--
-- A short remark about the type signature: the 'AnyFailure' is the type of failures such
-- a computation may emit during stepwise evaluation (during lazy evaluation, this is
-- simply a bottom value). Both 'String' and 'AnyFailure' are typical examples.
-- The 'I' type is the type of the progress reports. The watcher type is given seperately.
-- A computation may state how it is evaluated: either "may use lazy evaluation" (via the
-- type 'Lazy') or "use sequential evaluation" (via the type 'Sequential'). For most
-- computations this is not an issue: either keep it polymorphic (like in this example
-- via a universally quantified type variable), or use 'Lazy' (the preferred default
-- evaluation mode).
-- We also do not care about the watcher type for progress reports of type 'I'. Either
-- keep the type polymorphic, or simply choose a type like 'AnyWatcher' (or '()').
-- Finally, the last type of the value that evaluation of the computation results into.
-- The first three parameters to 'Stepwise' typically stay the same, the latter three
-- may vary from one computation to another.
test1 :: Stepwise AnyFailure I any AnyWatcher Int
test1 = do fail "x"  -- result is independent of this failure
           return 3

exp1Succs  = lazyEval test1      -- yields a value without need to evaluate the 'fail'
exp1Fails  = seqEval test1       -- encounters the failure before it delivers the result
exp1Fails2 = stepwiseEval test1  -- similar


-- | Test 2: verify that the selection process causes strict evaluation.
-- Despite running 'lazyEval' on 'test2', strict evaluation will be done on the
-- alternatives until a choice is made. In this case, both alternatives fail, so
-- the entire result fails.
-- Note that the '<|>'-implementation takes the first child that succeeds, or the
-- last child that fails (left-biased).

test2 :: Stepwise AnyFailure I any AnyWatcher Int
test2 = fail "x" <|> fail "y"  -- both alternatives fail

exp2Fails = lazyEval test2     -- cannot deliver a result


-- | Test 3: verify selection of alternatives.
-- The non-failure alternative is selected in this case.
-- The 'Lazy' annotation here we can use "because we can". A 'Lazy'-annotation is
-- in principle never required (you can in such cases keep it polymorphic), but if
-- possible, it's a good idea to do so, to make clear which computations should
-- preferably be evaluated lazily.
test3 :: Stepwise AnyFailure I Lazy AnyWatcher Int
test3 = fail "q" <|> return 3  -- one alternative succeeds

exp3Succs = lazyEval test3   -- delivers the result of the succeeding alternatives


-- Test 4: finite exploration.
-- Creates a possibly infinite computation. Each invocation of 'test4' emits an 'I' report,
-- and then either finished or recurses. Since '<|>' selects the shortest succeeding
-- alternative, it thus emits one 'I' report, then succeeds.
test4 :: Stepwise AnyFailure I any AnyWatcher Int
test4 = do emit I  -- emit an 'I' for each 'test4' invocation
           (test4 <|> return 3)  -- one alternative infinitely long, other only one step

exp4Succs = lazyEval test4   -- delivers the first right alternative


-- Test 5: verify online results.
-- This example builds on previous examples. It emits the first two elements of the
-- list. The third element, however, comes from a failing computation thus cannot
-- be produced. Using lazy evaluation, a partial result can be delivered (the first
-- two elements of the list). Using sequential evaluation, however, no result is
-- produced.
-- This outcome is independent from the sequential evaluations causes by 'test4' and
-- 'test2' in the process of computing an answer.

test5 :: Stepwise AnyFailure I any AnyWatcher [Int]
test5 = do x <- return 3
           y <- test4         -- potentially long computation
           z <- test2         -- failing computation
           return [x,y,z]

exp5Succs = lazyEval test5    -- delivers the first two numbers, then fails
exp5Fails = seqEval test5     -- fails, because of 'test2'


-- Test 6: 'sequentially' and 'lazily'.
-- This example demonstrates the use of both evaluation-mode transformers.
-- With 'sequentially', a sequential computation is embedded in a lazy
-- computation. With 'lazily' the other way around. It is important to
-- note that the lhs of a bind may not be needed in order to produce a
-- value for the rhs when in a lazy context, whereas this is not the
-- case for the sequential context.
test6 :: Stepwise AnyFailure I Lazy AnyWatcher Int
test6 = do a <- sequentially $ do liftIO (putStrLn "or not?")
                                  return 1
           x <- error "should not be referenced"
           g <- sequentially test4
           u <- lazily $ do y <- return 2
                            z <- error "should not be referenced too" 
                            return y
           h <- sequentially $ do liftIO (putStrLn "in this")
                                  liftIO (putStrLn "order?")
                                  return 3
           j <- error "this one also not"
           return (h+g+u+a)

exp6Succs = lazyEval test6


-- | Test 7: collecting multiple results.
--
-- 'test7b' generates paths: the left subpath is of length n-1 the right subpath is a lot shorter (n `div` 2)
--  (just for fun).
-- 'test7a' succeeds only for those paths that satisfy a funny criteria via xor. Those it returns.
-- When it succeeds, 'test7b' emits a progress report collecting that value.
-- 'merge' tries out options in a breadth-first way, and concatenates the lists in the progress reports.
-- 'test7c' takes out the list of all succeeding paths.
--
-- We collect these multiple results in a more informative form of progress report 'J'. The type of
-- the watcher is important here. The 'test7a' function does not make any assumptions about the
-- watcher, however 'test7b' does. When 'test7a' succeeds, it collects that results in a 'Collect'.
data J t = Collect [t]   -- collect results of type t
         | J

-- | We may not make an assumption about the watcher here, hence we keep the watcher type polymorphic.
test7a :: [Bool] -> Stepwise AnyFailure J Lazy somewatcher [Bool]
test7a xs = do emit J
               let outcome = foldr (\l r -> l `xor` r) True xs
                   xor a b = (a || b) && not (a && b)
               sequentially $ do liftIO (putStrLn ("branch: " ++ show xs ++ ": " ++ show outcome))
                                 if outcome
                                  then return xs
                                  else fail "bohoo"

test7b :: Int -> [Bool] -> Stepwise AnyFailure J Lazy [Bool] ()
test7b 0 path = test7a path >>= \xs -> emit (Collect [xs]) >> return ()
test7b n path = let l = test7b (n-1) (False : path) 
                    r = test7b (n `div` 2) (True : path)
                in do emit J
                      merge l r

merge :: Stepwise AnyFailure J Lazy [Bool] () -> Stepwise AnyFailure J Lazy [Bool] () -> Stepwise AnyFailure J Lazy [Bool] ()
merge l r = merge' (localStep l) (localStep r) where
  merge' (Step J p) (Step J q) = do
    emit J
    merge p q
  merge' (Failed _) p  = task p
  merge' p (Failed _)  = task p
  merge' (Step J p) q  = merge p (task q)
  merge' p (Step J q)  = merge (task p) q
  merge' (Step (Collect l) _) (Step (Collect r) _) = do
    emit (Collect (l ++ r))
    return ()

-- | Strips steps (thus evaluates sequentially), until it hits a 'Collect' message, which is subsequently
-- delivers.
test7c :: Stepwise AnyFailure J Lazy a [[Bool]]
test7c = hunt $ test7b 5 [] where
  hunt m = hunt' (localStep m)
  hunt' (Failed _) = fail "shouldnt happn"
  hunt' (Step J p) = hunt p
  hunt' (Step (Collect xs) _) = return xs

test7Succs = lazyEval test7c


-- | Test 8: lookahead.
-- Decisions taken in this example may depend on what happens in the continuation.
-- We takes as example path-finding in a labyrinth. Taking a step that brings us
-- back to a position where we've been before is an immediate failure. However,
-- the possibilities that remain may hit a dead-end later.
type Lab a = Stepwise AnyFailure I Lazy AnyWatcher a
type Pos = (Int,Int)
data Dir = North | East | South | West deriving (Enum, Show)
type Path = [Dir]
data LabState = LS
  { ls_lab   :: !(Set Pos)        -- the empty squares in the labyrinth
  , ls_end   :: !Pos
  , ls_cur   :: !Pos
  , ls_trail :: !(Set Pos)        -- a trail of crumbs of where we've been before.
  , ls_path  :: Path -> Path      -- the path traversed so far
  }

test8_search :: LabState -> Lab LabState
test8_search st0
  = test8_finished st0
      `test8_best`
    test8_non [ do st1 <- test8_move d st0
                   test8_search st1
              | d <- [North .. West] ]

test8_finished :: LabState -> Lab LabState
test8_finished st0 =
  let p = ls_cur st0
      e = ls_end st0
  in if p == e
     then return st0
     else fail "not at the end yet"

test8_non :: [Lab a] -> Lab a
test8_non []     = fail "choose something else"
test8_non (x:xs) = x `test8_best` (test8_non xs)

test8_move :: Dir -> LabState -> Lab LabState
test8_move d st0 =
  let p  = ls_cur st0
      p' = test8_forward d p
      a  = ls_lab st0
  in sequentially $ do
       liftIO (putStrLn ("Trying: " ++ show d ++ " @ " ++ show p' ++ " from " ++ show (ls_cur st0)))
       if p' `Set.member` a   -- can we go to this square?
         then let ps = ls_trail st0
              in if p' `Set.member` ps
                 then fail "already been there"
                 else do emit I
                         return st0 { ls_cur   = p'
                                    , ls_trail = Set.insert p' (ls_trail st0)
                                    , ls_path  = (d :) . ls_path st0 }
         else fail "at non-empty square"

test8_forward :: Dir -> Pos -> Pos
test8_forward North (x,y) = (x,y+1)
test8_forward East  (x,y) = (x+1,y)
test8_forward South (x,y) = (x,y-1)
test8_forward West  (x,y) = (x-1,y)

-- | This data type represents a handle to a stepwise computation, and the intended continuation
--   when we are finished evaluating that computation.
data ContHandle o w a where
  ContHandle :: (StepHandle AnyFailure I Lazy AnyWatcher b) -> Bool -> (b -> Stepwise AnyFailure I o w a) -> ContHandle o w a

test8_best :: Lab a -> Lab a -> Lab a
test8_best l r = sequentially $ do
  hl <- handle l
  hr <- handle r
  lookahead (\k -> sequentially $ best (ContHandle hl False k) (ContHandle hr False k))
  where
    best c1@(ContHandle hl commitl kl) c2@(ContHandle hr commitr kr) = do
      perform hl         -- demonstrates the monadic interface to steps
      perform hr         -- (it's main advantage is that it is explicitly
      rl <- report hl    -- made clear what the relative ordering between
      rr <- report hr    -- steps is
      liftIO (putStrLn "comparing...")
      let inspect (Progress I) (Progress I) = do liftIO $ putStrLn "progres"
                                                 -- emit I >> best c1 c2
                                                 best c1 c2
          inspect _ (Failure _) = do liftIO $ putStrLn "failure right"
                                     proceedAfter hl kl
          inspect (Failure _) _ = do liftIO $ putStrLn "failure left"
                                     proceedAfter hr kr
          inspect (Finished v) _ | commitl   = do liftIO $ putStrLn "committed left"
                                                  lazily $ kl v
                                 | otherwise = do liftIO $ putStrLn "finished left"
                                                  h' <- handle $ recode $ kl v
                                                  let c' = ContHandle h' True return
                                                  best c' c2
          inspect _ (Finished v) | commitr = do liftIO $ putStrLn "committed right"
                                                lazily $ kr v
                                 | otherwise = do liftIO $ putStrLn "finished right"
                                                  h' <- handle $ recode $ kr v
                                                  let c' = ContHandle h' True return
                                                  best c1 c'
          inspect (Future f) _ = do liftIO $ putStrLn "future left"
                                    h' <- handle $ recode $ f $ lazily . kl
                                    let c' = ContHandle h' commitl return
                                    best c' c2
          inspect _ (Future f) = do liftIO $ putStrLn "future right"
                                    h' <- handle $ recode $ f $ lazily . kr
                                    let c' = ContHandle h' commitr return
                                    best c1 c'
                                    
      inspect rl rr

    recode = unsafeObserve -- observe (\I -> I) -- in this particular case, with GHC, it is safe to use: unsafeObserve (which is faster)
    proceedAfter h k = recode (proceed h) >>= lazily . k

-- | Example labyrinth
test8_lab1 :: [Pos]
test8_lab1 = [(0,0),(1,0),(1,1)]
-- [(x,x) | x <- [0..1]] ++ [(x+1,x) | x <- [0..1]] -- ++ [(x+2,x) | x <- [0..9]]

exp8Succ :: Path
exp8Succ = ls_path sFin []
  where
    sIn = LS { ls_lab   = Set.fromList test8_lab1
             , ls_cur   = (0,0)
             , ls_end   = (1,1)
             , ls_trail = Set.empty
             , ls_path  = id
             }
    sFin = stepwiseEval (test8_search sIn)


-- | Test 9: Explicit sharing.
-- This example builds on the previous one. Since we immediately fail when a step
-- would take us back at a position that we've been before, the paths we traverse
-- form a DAG. However, certain paths on this DAG we may traverse more than once.
-- In this example, we ensure that we only traverse each path once.

-- Todo: compression and parallel evaluation.
