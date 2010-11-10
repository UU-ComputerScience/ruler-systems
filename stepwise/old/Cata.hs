-- | Some combinators to express breadth-first evaluation of catamorphisms,
-- which allows you to stepwise evaluate the results of children. If,
-- during the evaluation of an alternative, a choice needs to be made between taking
-- the results of its children, with these combinators, you can stepwise
-- evaluate the children in parallel, until a choice can be made.
-- Until a choice is made, evaluation proceeds strictly; after a choice
-- is made, evaluation proceeds lazily.
-- What constitutes to be a step is determined by the callee.

{-# LANGUAGE GADTs, TypeFamilies, EmptyDataDecls, RankNTypes, MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}
module Data.BreadthFirstCata.Cata
  (Child(Child),Inh,Syn,Comp
  ,invoke,final,info,resume
  ,inject,sem_Inject,Inject,lazyEval,oneStep,Outcome(Step,Fin)) where


-- | Semantics of a child of type @n@ as a function from inherited
-- attributes (@Inh n@) to a computation @Comp i n@ of synthesized attributes (@Syn n@).

newtype Child i n = Child (Inh n -> Comp i n)
data family Inh n :: *         -- index @n@ uniquely determines the type of the inherited and
data family Syn n :: *         -- synthesized attributes.


-- | Computation of synthesized attributes of nonterminal of type @n@.
-- It is a trace of @Info@-effects, that keeps track of the intermediate
-- states of the tree (using @Pending@), ending ultimately in the
-- synthesized values (using @Final@).
-- Operationally, we lift @Info@-values over @Pending@-values, thereby
-- gradually rewriting the latter, until it results in a @Final@.

data Comp i n where
  Pending :: !(Parents i n' n) -> Comp i n' -> Comp i n
  Final   :: !(Syn n) -> Comp i n
  Info    :: !i -> Comp i n -> Comp i n


-- | Given a node of type @m@ that is currently being evaluated for a
-- tree of type @n@, @Parents i m n@ is the stack of its parent nodes
-- (that wait for values of their children to continue evaluation).
-- The outermost @Cont@-value) is the top of the stack, i.e. the direct
-- parent of the formerly mentioned node.

data Parents i n' n where
  Cont :: !(Node i n' k) -> !(Parents i k n) -> Parents i n' n
  Root :: Parents i n n


-- | Explicit closure for a node that expects results of a child of type @m@
-- in order to continue evaluation. The state of a node consists of the
-- (still needed) results of already constructed children and local
-- attributes.

newtype Node i n' n = Node (Syn n' -> Comp i n)


-- | Lazy evaluation of a computation.
-- Note: we cannot inspect the effect-trace, as it would sequentialize
-- the evaluation of children.

lazyEval :: Comp i n -> Syn n
lazyEval (Pending stack r) = evalParents stack (lazyEval r)
lazyEval (Final v)  = v
lazyEval (Info _ r) = lazyEval r

evalParents :: Parents i n' n -> Syn n' -> Syn n
evalParents Root v = v
evalParents (Cont (Node f) c) v = evalParents c (lazyEval (f v))


-- | Result of one step evaluation: either the final result
-- or an updated computation. What constitutes to a step
-- depends on the application: evaluation proceeds until
-- an outcome can be given.

data Outcome i n
  = Fin  !(Syn n)
  | Step !i (Comp i n)


-- | One step strict evaluation. Reduction proceeds until
-- the computation is either finished or yields an @Info@-effect.

oneStep :: Comp i n -> Outcome i n
oneStep (Pending stack r)
  = case oneStep r of
      Fin v     -> oneStep (reduceParent stack v)
      Step i r' -> Step i (Pending stack r')
oneStep (Final v)  = Fin v
oneStep (Info i r) = Step i r

reduceParent :: Parents i n' n -> Syn n' -> Comp i n
reduceParent Root v = Final v
reduceParent (Cont (Node f) c) v = Pending c (f v)


-- | Unwraps a @Child@
invoke :: Child i n -> Inh n -> Comp i n
invoke (Child f) inh = f inh


-- | Wrapper for final result.
final :: Syn n -> Comp i n
final = Final

-- | Wrapper for an effect.
info :: i -> Comp i n -> Comp i n
info = Info

-- | Create a |Pending| computation that waits for the given computation.
resume :: Comp i n' -> (Syn n' -> Comp i n) -> Comp i n
resume c !k = Pending (Cont (Node k) Root) c


-- | Injection of steps as conventional call

data Inject
data instance Inh Inject = Inh_Inject {}
data instance Syn Inject = Syn_Inject {}

sem_Inject :: Child i Inject
sem_Inject = Child (const $ final $ Syn_Inject {})

inject :: i -> Comp i Inject
inject i = info i $ final $ Syn_Inject {}
