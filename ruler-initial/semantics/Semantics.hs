{-# LANGUAGE KindSignatures, GADTs, ScopedTypeVariables, FlexibleInstances, TypeSynonymInstances, DeriveDataTypeable, FlexibleContexts, UndecidableInstances, PatternSignatures #-}
module Semantics where

--
-- Bigstep operational semantics for the Ruler Core language. This semantics
-- is executable. A successful execution yields (an abstract representation of)
-- a type derivation.
--
-- This code file is organized in several parts. It begins with a definition
-- of the Ruler Core language. Then come several untility functions useful for
-- manipulating the objects of this language. The semantics itself is given as
-- state transitions (where the state is actually hidden using monad 'I'). The
-- remainder of the file is an example: a type system encoded in this Ruler
-- Core language, an example expression, and some support code (dealing with
-- pretty printing and unification).
--


import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Typeable
import Data.Maybe
import Data.List
import Control.Monad.State.Strict
import Control.Monad.Error
import Debug.Trace


--
-- Ruler Core language definition.
--


type Scheme = String
type Ident  = String
type Var    = Int
type Scope  = Int

-- A Ruler Core program is just a set of rules. Rules belong to a scheme (which
-- declares the input and output parameters of a rule), but these schemes are
-- are not written down in Ruler Core.
-- Each rule may have several premises and one conclusion. A premise can be an
-- arbitrary Statement, but the conclusion is requried to be a StmtScheme. This
-- StmtScheme tells to which scheme this rule belongs, to what name the input
-- parameters are bound, and under what name the output parameters are computed.
-- The rules are required to be syntax-directed. There must be one applicable
-- rule for any instantiation of a scheme.
-- A rule also has a unique name.
type Rules = Set Rule
data Rule :: * where
  Rule :: Ident -> Statements -> Statement -> Rule

instance Ord Rule where
  compare (Rule name _ _) (Rule name' _ _) = compare name name'

instance Eq Rule where
  (Rule name _ _) == (Rule name' _ _) = name == name'

-- Statements come in several forms.
-- The following statements are classical:
-- * A StmtScheme represents an invocation of a scheme. When used as
--   conclusion, the input-bindings define under what name the inputs of the
--   scheme are available to the Statements of the rule, and the output-bindings
--   define which of the produced values by the Statements are the outputs of
--   the scheme.
-- * StmtUnify represents the unification of two named values. The values
--   bound to these names must be of the same type and an instance of Unifyable
--   available for them.
-- * Stmtxec allows Haskell code to act as statement. This way, functionality
--   such as lookups in environments or (de)construction of values can be
--   represented. The access to the values produced by other statements, as well
--   as producing new values, and success/failure, can be dealt with by means of
--   a simple API.
-- Then some statements for dealing with declarative aspects:
-- * StmtDefer delays the execution of the statements-list it is parameterized
--   with. These statements produce values for outputs 'Ident' and '[Ident]'.
--   A Deferred value (placeholder) is assigned to the 'Ident' and the execution
--   is only performed when a commit is done on this placeholder. For the outer
--   outputs (the [Ident]), a Bottom value is assigned until the deferred
--   statements replace them.
-- * StmtCommit commits a certain named value to a variable. This fixes the
--   decision about what this variable should be. This decision cannot be
--   reverted. Then the deferred statements in the list are tried from head to
--   tail. One of them must succeed.
-- * StmtFixate introduces a scope: all unresolved deferred decisions
--   introduced in this scope are resolved when leaving the scope. On each
--   unresolved decision a commit is done with a fresh value. Then, all
--   remaining fresh values are converted to a Fixed value (meaning that the
--   value is not deferred anymore, but there are no assumptions about the
--   contents).
-- Finally StmtFixpoint represents an exhaustive application of some
-- transitive rules (of which the ouptput can be remapped to the input). It
-- finds the fixpoint by looking at the heap values for the identifier given
-- as parameter.
type Statements = [Statement]
data Statement :: * where
  StmtScheme   :: Bindings -> Bindings -> Scheme -> Statement
  StmtUnify    :: Ident -> Ident -> Statement
  StmtExec     :: I () -> Statement
  StmtDefer    :: Ident -> [Ident] -> [Statements] -> Statement
  StmtCommit   :: Var -> Ident -> Statement
  StmtFixate   :: Statement -> Statement
  StmtFixpoint :: Ident -> Bindings -> Statement -> Statement

type Bindings = Map Ident Ident   -- Bindings map names to names.
type Heap = Map Ident Value       -- The heap binds values to names.

-- For a type variable, the substitution can track one of the following
-- possibilities:
-- * A concrete value.
-- * A deferred statement, which has the statements to choose from, a
--   reference to the heap, and some other meta data (a name for the
--   derivation and the scope, respectively).
-- * A Bottom value (basically a black hole).
type Substitution = Map Var SubstEntry
data SubstEntry :: * where
  SubstValue  :: Value -> SubstEntry
  SubstDefer  :: Var -> Scope -> Heap -> [Statements] -> SubstEntry
  SubstBottom :: SubstEntry

-- A substitution can be applied to instances of 'Container'. Every value that
-- somehow stores a deferred value needs to be an instance of this class.
class Container a where
  appSubst :: Substitution -> a -> a
  appSubst = const id

-- Values that can participate as the output of "defer" must be an instance of
-- this class. Deferrable values have special values representing yet-unknown
-- information, and information that is required to be unknown. "deferVars"
-- returns all deferred values in this value. This is needed for example to
-- perform the occur-check. "deferValue" produces a deferred value for the given
-- unique identifier. "fixedValue" produced a fixed value for the given unique
-- identifier.
class Deferrable a where
  deferVars  :: a -> [Var]
  deferValue :: Var -> a
  fixedValue :: Var -> a

-- Values passed to
class Unifyable a where
  unify :: a -> a -> I ()

-- Represents an abstract value. Abstract values can be a concrete value of
-- some type (that must be typeable and a container, such that we always can
-- perform appSubst on it). Also, it can have some extra catabilities,
-- registered in the Prop-list (such as Unifyable or Deferrable).
data Value :: * where
  Value    :: (Show v, Eq v, Typeable v, Container v) => [Prop v] -> v -> Value
  Deferred :: Var -> Value
  Fixed    :: Var -> Value

-- A property of a value. A property stores a dictionary, which can be used on
-- concrete values after the pattern match. We often need this when we don't
-- know the actual type of the concrete value but want to use some of these
-- operations like "unify" if it has this as capability.
data Prop :: * -> * where
  PropDeferrable :: Deferrable v => Prop v
  PropUnifyable  :: Unifyable v => Prop v

-- Returns the PropDeferrable property that should be in the list. Subsequently,
-- one can pattern match on it, such that it brings the Deferrable-dictionary in
-- scope.
extractDeferrable :: [Prop v] -> Prop v
extractDeferrable ((p@PropDeferrable) : _) = p
extractDeferrable (_:xs)                   = extractDeferrable xs

-- Returns the Unifyable property that should be in the list.
extractUnifyable :: [Prop v] -> Prop v
extractUnifyable ((p@PropUnifyable) : _) = p
extractUnifyable (_:xs)                  = extractUnifyable xs

instance Show Value where
  show (Value _ v)  = show v
  show (Deferred i) = "<deferred: " ++ show i ++ ">"
  show (Fixed i)    = "<fixed: " ++ show i ++ ">"

instance Eq Value where
  (Value _ v1) == (Value _ v2)
    = case cast v2 of
        Just v2' -> v2' == v1
        Nothing  -> False
  (Deferred v1) == (Deferred v2) = v1 == v2
  (Fixed v1)    == (Fixed v2)    = v1 == v2
  _ == _ = False


instance Container Substitution where
  appSubst s s' = Map.union s (Map.map (appSubst s) s')

instance Container SubstEntry where
  appSubst s (SubstValue v) = SubstValue (appSubst s v)
  appSubst _ other          = other

instance Container Heap where
  appSubst s = Map.map (appSubst s)

instance Container Value where
  appSubst s (Value props v)  = Value props $ appSubst s v
  appSubst s d@(Deferred var)
    = case Map.lookup var s of
        Just (SubstValue v) -> v
        _                   -> d
  appSubst s d@(Fixed var)
    = case Map.lookup var s of
        Just (SubstValue v) -> v
        _                   -> d

instance Container Var
instance Container a => Container [a] where
  appSubst s = map (appSubst s)

-- Flip the bindings around. So "e -> a" now becomes "a -> e".
reverseBindings :: Bindings -> Bindings
reverseBindings = Map.fromList . map (uncurry (flip (,))) . Map.toList

-- Foreach binding "n -> m", take value m from the heap and store it under
-- the name n.
takeWithRenaming :: Bindings -> Heap -> Heap
takeWithRenaming bindings heap = Map.fromList [ (n, Map.findWithDefault (error ("takeWithRenaming: value not in heap: " ++ show n')) n' heap) | (n,n') <- Map.toList bindings ]


-- Inference monad. Represents state transitions labelled with "a". Of interest
-- for "a" is only Deriv, representing a derivation. All other types for "a"
-- are internal.
type I a = ErrorT Err (State IState) a
data IState = IState { allRules :: Rules, uniqueCounter :: Var, currentSubst :: Substitution, currentHeap :: Heap, currentScope :: Scope, deferredDerivs :: Map Var [Deriv] }

-- Policy for what to do when there is possibly more than one applicable rule.
-- We provide two policies: use the first one that succeeds, or use all that
-- succeed.
type OverlapPolicy = [I Deriv] -> I Deriv

-- Custom error type: a string annotated with a flag that indicates if the
-- error should abort the type inferencing. For pattern matches, this is not
-- the case, then we want to try some other possibilities first. But for
-- all other errors, we want to abort type inferencing with an error.
data Err :: * where
  Err :: String -> Bool -> Err
  deriving Show

instance Error Err where
  noMsg  = Err "An underspecified error has occured." False
  strMsg = flip Err False

-- Throws an error from which the inferencing can recover (by catching it and
-- trying some other rule if this one exists).
throwErrorRecoverable :: String -> I a
throwErrorRecoverable = throwError . flip Err True

-- Throws an error that requires the inferencing to stop with the given
-- message.
throwErrorAbort :: String -> I a
throwErrorAbort = throwError . flip Err False

-- Turns all irrecoverable errors in recoverable ones.
recoverable :: I a -> I a
recoverable m = m `catchError` (\(Err msg _) -> throwError (Err msg True))

-- Continues only with the inferencing if the predicate is True.
continueIff :: Bool -> I ()
continueIff True  = return ()
continueIff False = throwErrorRecoverable "continueIff: predicate failed."

-- Abstract representation of derivations. Portions of a derivation may not be
-- available yet because they are produced when executing a deferred statement.
-- A derivation uses "Rerference" to refer to a named derivation that may not
-- yet be there. This name is stores in the "SubstDefer" value, such that when
-- a commit is performed, the named derivation can be stored.
-- An Opaque derivation is a derivation of which we don't know how to visualize
-- it (i.e. the result of an Exec). A Node-derivation is produced for each
-- invocation of a StateScheme, and a Transtive-derivation when using Fixpoint.
data Deriv :: * where
  Opaque     :: Deriv
  Node       :: String -> Heap -> [Deriv] -> Deriv
  Reference  :: Var -> Deriv
  Transitive :: [Deriv] -> Deriv

instance Container Deriv where
  appSubst subst (Node name heap derivs) = Node name (appSubst subst heap) (map (appSubst subst) derivs)
  appSubst subst (Transitive derivs)     = Transitive (map (appSubst subst) derivs)
  appSubst _     d                       = d

instance Show Deriv where
  show Opaque        = "<<opaque>>"
  show (Reference v) = "<<reference " ++ show v ++ ">>"
  show (Node name heap derivs)
    = "rule " ++ name ++ "\n" ++ indent (showHeap heap ++ "\n" ++ intercalate "\n" (map show derivs))
  show (Transitive derivs)
    = "transitive\n" ++ indent (intercalate "\n" $ map show derivs)

indent :: String -> String
indent = unlines . map ("  " ++) . lines

showHeap :: Heap -> String
showHeap heap = "[" ++ intercalate ", " [ n ++ " := " ++ show v | (n,v) <- Map.toList heap] ++ "]"

-- Stores some named derivations in the internal state.
addDerivs :: Var -> [Deriv] -> I ()
addDerivs v d = modify (\s -> s { deferredDerivs = Map.insert v d (deferredDerivs s) })

-- Glues the derivations together. This can only be done after all deferred
-- statements have been executed, otherwise they are not in the map that is
-- stored in the state yet.
composeDeriv :: Deriv -> I Deriv
composeDeriv deriv
  = do subst <- gets currentSubst
       let compose Opaque              = return []
           compose (Transitive derivs) = fmap (return . Transitive . concat) $ mapM compose derivs
           compose (Node name heap derivs)
             = do derivs' <- mapM compose derivs
                  return [ Node name heap (concat derivs') ]
           compose (Reference var)
             = do derivsMap <- gets deferredDerivs
                  derivs    <- mapLookup var derivsMap
                  fmap concat $ mapM compose derivs
       fmap head $ compose (appSubst subst deriv)

-- Lookups a value in the map, and throws an irrecoverable error if the key is
-- not in there.
mapLookup :: (Show k, Ord k) => k -> Map k v -> I v
mapLookup k map
  = case Map.lookup k map of
      Just v -> return v
      _      -> throwErrorAbort ("mapLookup: key " ++ show k ++ " not in map.")

-- Tries to execute one of the given alternative inferences. If a recoverable
-- error occurs, it backtracks to the next alternative. If an irrecoverable
-- error occurs or no alternatives are left, the execution fails.
tryInferOne :: [I a] -> I a
tryInferOne = foldr takeOrContinue (throwErrorAbort "tryInferOne: tried all alternatives without success")
  where takeOrContinue i r
          = do mv <- tryInfer i
               maybe r return mv

-- Executes all given inferences. Inferences that fail with a recoverable error
-- are ignored. The "conc" function combines the results.
tryInferAll :: ([a] -> a) -> [I a] -> I a
tryInferAll conc = fmap conc . foldr tryAndContinue (return [])
  where tryAndContinue i r
          = do mv <- tryInfer i
               vs <- r
               return $ maybe vs (:vs) mv

-- Executes an inference, returning in a Just a if the inference succeeds, or
-- a nothing when a recoverable error occurs (state is not changed), or in an
-- abort if an irrecoverable error occurs.
tryInfer :: I a -> I (Maybe a)
tryInfer i
  = do subst <- gets currentSubst
       heap  <- gets currentHeap
       scope <- gets currentScope
       (i >>= return . Just) `catchError`
         (\e@(Err _ recoverable) ->
           if recoverable
           then do modify (\s -> s { currentSubst = subst, currentHeap = heap, currentScope = scope })
                   return Nothing
           else throwError e
         )

-- Executes an inference on a copy (transformed by f1) of the current heap. If
-- the execution is successful, the final heap is produced by f2 parameterized
-- with the old heap and the new heap. Effectively, the inference is run in a
-- subscope, where fTo and fFrom control the scoping.
scopeHeap :: (Heap -> Heap) -> (Heap -> Heap -> Heap) -> I a -> I a
scopeHeap fTo fFrom m
  = do heap <- gets currentHeap
       modifyHeap fTo
       v     <- m `catchError` (\e -> modifyHeap (const heap) >> throwError e)
       heap' <- gets currentHeap
       subst <- gets currentSubst
       modifyHeap (fFrom $ appSubst subst heap)
       return v

-- Applies a heap-transformer to the heap stored in the state.
modifyHeap :: (Heap -> Heap) -> I ()
modifyHeap f = modify (\s -> s { currentHeap = f (currentHeap s) })

-- Looksup an identifier from the heap and returns the abstract value
-- representation. This abstract value can be a concrete value of some type,
-- or some abstract deferred/fixed value.
heapLookup :: Ident -> I Value
heapLookup ident
  = do heap <- gets currentHeap
       case Map.lookup ident heap of
         Just v -> return v
         _      -> throwErrorAbort ("heapLookup: not in heap: " ++ show ident)

heapDelete :: Ident -> I ()
heapDelete n
  = modify (\s -> s { currentHeap = Map.delete n (currentHeap s) })

-- Inserts the abstract value for an identifier in the heap. If a value already
-- exists in the heap, it's considered an error unless it's a deferred value,
-- in which case the substitution is updated.
heapStore :: Ident -> Value -> I ()
heapStore n v
  = do heap <- gets currentHeap
       case Map.lookup n heap of
         Just (Deferred i) -> addSubst $ Map.singleton i (SubstValue v)
         (Just v)          -> throwErrorAbort ("heapStore: ident " ++ n ++ " with value " ++ show v ++ " already in heap (and not deferred).")
         Nothing           -> return ()
       modify (\s -> s { currentHeap = Map.insert n v (currentHeap s) })

-- Extract the concrete value a from the abstract value. For values that may
-- be deferred or fixed, pass a PropDeferrable as Prop a parameter, because
-- the corresponding Deferrable-instance is used to construct a concrete
-- deferred or fixed value from the abstract value. The reason why this
-- information needs to be passed also when unpacking is because the type may
-- be unknown at the moment of constructing the abstract value.
unpack :: Typeable a => [Prop a] -> Value -> I a
unpack _ (Value _ v) =
  case cast v of
    Just r -> return r
    _      -> throwErrorAbort "unpack: cast failed"
unpack props (Deferred i) =
  case extractDeferrable props of
    PropDeferrable -> return (deferValue i)
unpack props (Fixed i) =
  case extractDeferrable props of
    PropDeferrable -> return (fixedValue i)

-- Wraps a concrete value a in an abstract value. Abstract values can have a
-- couple of properties which are passed as Prop a values. Unifyable is one of
-- these important properties.
pack :: (Show a, Eq a, Typeable a, Container a) => [Prop a] -> a -> I Value
pack props v = return $ Value props v

-- Loads a value that has certain properties. Actually, the only property of
-- interest is the PropDeferrable property. It is possible that the value to
-- load from the heap hasn't a concrete type yet (it's a Fixed/Deferrable
-- unknown). Then we need the Deferrable-property to convert it to a proper
-- value in the type a.  If this property isn't there then "unpack" fails.
load :: Typeable a => Ident -> [Prop a] -> I a
load ident props = heapLookup ident >>= unpack props

-- Loads a plain value. Such a value must be stored fully determined in the
-- heap.
loadF :: Typeable a => Ident -> I a
loadF ident = load ident []

-- Loads a value that may potentially be a deferred value.
loadD :: (Typeable a, Deferrable a) => Ident -> I a
loadD ident = load ident [PropDeferrable] -- loads a potentially deferred value

-- Store a value of which the type is known, including some of its properties.
-- This function is called by storeF and storeUD, which are probably easier to
-- use.
store :: (Show a, Eq a, Typeable a, Container a) => Ident -> [Prop a] -> a -> I ()
store ident props v = pack props v >>= heapStore ident

-- Store a concrete value. Such a value cannot be unified or be used as part of
-- the deferrable mechanism.
storeF :: (Container a, Typeable a, Eq a, Show a) => Ident -> a -> I ()
storeF ident = store ident []

-- Store a value that can be unified or be used as part of the deferrable
-- mechanism.
storeUD :: (Container a, Typeable a, Eq a, Show a, Unifyable a, Deferrable a) => Ident -> a -> I ()
storeUD ident = store ident [PropDeferrable, PropUnifyable]

-- Produces a unique variable from the internal counter.
unique :: I Var
unique = do uniq <- gets uniqueCounter
            modify (\s -> s { uniqueCounter = 1 + uniqueCounter s })
            return uniq

-- Produces a fresh variable. A fresh variable has a succeeds-always (or
-- identity) deferrable statement in the substitution. If a commit occurs on it,
-- it will succeed.
fresh :: I Var
fresh = do v <- unique
           scope <- gets currentScope
           let d = SubstDefer v scope Map.empty [[]]
           modify (\s -> s { currentSubst = Map.insert v d (currentSubst s) })
           return v

-- Executes the given inferencer in a subscope.
incScope :: I a -> I a
incScope m
  = do prevScope <- gets currentScope
       modify (\s -> s { currentScope = 1 + prevScope })
       a <- m
       modify (\s -> s { currentScope = prevScope })
       return a

-- Add a substitution to the substitution in the state.
addSubst :: Substitution -> I ()
addSubst subst = modify (\s -> s { currentSubst = appSubst subst (currentSubst s) })

-- Generic equality function that deals with the Deferred/Fixed cases, and
-- delegates to the normal structural equality for the data-type specific
-- equality checks. The cases "variable" with "concrete value" also needs to
-- be handled by the data-type specific equality check.
equal :: Value -> Value -> I ()
equal (Deferred var1) v@(Fixed _) = commit var1 v
equal v@(Fixed _) (Deferred var2) = commit var2 v
equal (Deferred var1) (Deferred var2)
  | var1 == var2                  = return ()
  | otherwise                     = 
      do subst  <- gets currentSubst
         (SubstDefer derivRef3 scope3 heap3 statementss3) <- mapLookup var1 subst
         (SubstDefer derivRef4 scope4 heap4 statementss4) <- mapLookup var2 subst
         var3   <- fresh
         var4   <- fresh
         let scope = scope3 `min` scope4
             defer3' = SubstDefer derivRef3 scope heap3 statementss3
             defer4' = SubstDefer derivRef4 scope heap4 statementss4
             heap = Map.fromList [("v2",Deferred var2),("v3",Deferred var3),("v4",Deferred var4)]
         addSubst $ Map.fromList
           [ (var2, SubstDefer var2 scope heap [[ StmtCommit var3 "v2", StmtCommit var4 "v3", StmtUnify "v2" "v4" ]])
           , (var3, defer3'), (var4, defer4'), (var1, SubstValue $ tyMkValue $ deferValue var2) ]
equal (Fixed i) (Fixed j) | i == j = return ()
equal (Value props val) value      = equalVal props val value
equal value (Value props val)      = equalVal props val value
equal _ _                          = throwErrorAbort "equal: undetermined/fixed values cannot be unified"

-- Delegates the equality check to the data-type specific equality function.
-- Here we know know the exact type of the value, such that if we perform an
-- equality check with a value of which we didn't know the type yet (because its
-- still a Deferred or Fixed unknown), then we know it know and can convert it
-- to such a value.
equalVal :: (Typeable a) => [Prop a] -> a -> Value -> I ()
equalVal props (v1 :: a) value
  = do v2 <- toVal value
       case extractUnifyable props of
         PropUnifyable -> unify v1 v2
  where
    toVal :: Value -> I a
    toVal (Value _ v) = return $ fromJust $ cast v
    toVal (Fixed v)
      = case extractDeferrable props of
          PropDeferrable -> return $ fixedValue v
    toVal (Deferred v)
      = case extractDeferrable props of
          PropDeferrable -> return $ deferValue v

-- Match up the parameters pairwise (param1 with param2, param3 with param4)
equalPairwise :: (Unifyable a, Unifyable b, Container a, Container b) => a -> a -> b -> b -> I ()
equalPairwise t1 t2 t3 t4
  = do unify t1 t2
       subst <- gets currentSubst
       unify (appSubst subst t3) (appSubst subst t4)

-- Commits a deferred variable to a type. This choice for the variable is final.
-- No backtracking is performed if a better choice comes along. The value to
-- commit to may contain variables inside though.
-- If the variable to commit to is not deferred anymore (but a concrete value),
-- then this value must be equal to the value to commit to (since, we never go
-- back on a decision).
commit :: Var -> Value -> I ()
commit var value
  = do subst <- gets currentSubst
       entry <- mapLookup var subst
       case entry of
         SubstDefer derivRef _ heap statementss
           -> do addSubst (Map.singleton var (SubstValue value))
                 subst  <- gets currentSubst
                 derivs <- scopeHeap 
                             (const $ appSubst subst heap)
                             const
                             $ do deriv  <- tryInferOne $ map (mapM $ inferStatement tryInferOne) statementss
                                  heap'  <- gets currentHeap
                                  subst' <- gets currentSubst
                                  sequence_ $ Map.elems $ Map.intersectionWith updateSubst (appSubst subst heap) heap'
                                  return deriv
                 addDerivs derivRef derivs
         SubstValue value' -> equal value' value
         SubstBottom       -> throwErrorAbort "commit: cannot commit on a bottom-value."
  where
    updateSubst :: Value -> Value -> I ()
    updateSubst (Deferred v) val@(Value _ _) = addSubst $ Map.singleton v $ SubstValue val
    updateSubst _ _ = return ()

--
-- Operational semantics.
-- Given as state transitions of the form: state, statement -> state'. However,
-- the state is hidden in the monad 'I'. The state transitions are labelled with
-- derivations (i.e. a successful transition yields a derivation as
-- side-effect).
--

inferStatement :: OverlapPolicy -> Statement -> I Deriv
inferStatement combine (StmtScheme bindingsIn bindingsOut scheme)
  = do rules <- gets allRules
       combine [ scopeHeap (takeWithRenaming (reverseBindings bindingsIn') . takeWithRenaming bindingsIn)
                           (\heap heap' -> takeWithRenaming (reverseBindings bindingsOut) (takeWithRenaming bindingsOut' heap') `Map.union` heap)
                           $ do derivs <- mapM (inferStatement tryInferOne) premisses
                                heap   <- gets currentHeap
                                return $ Node name heap derivs
               | (Rule name premisses (StmtScheme bindingsIn' bindingsOut' scheme')) <- Set.toList rules
               , scheme == scheme'
               ]
inferStatement _ (StmtUnify identA identB)
  = do v1 <- heapLookup identA
       v2 <- heapLookup identB
       equal v1 v2
       subst <- gets currentSubst
       modifyHeap (appSubst subst)
       return Opaque
inferStatement _ (StmtExec f)
  = do f
       subst <- gets currentSubst
       modifyHeap (appSubst subst)
       return Opaque
inferStatement _ (StmtDefer ident idents statementss)
  = do derivVar <- unique
       vars  <- mapM (const fresh) idents
       addSubst $ Map.fromList $ zip vars (repeat SubstBottom)
       sequence_ $ zipWith (\i -> heapStore i . Deferred) idents vars
       var   <- fresh
       heap  <- gets currentHeap
       scope <- gets currentScope
       let heapDeferred = Map.insert ident (Deferred var) heap
       addSubst (Map.singleton var (SubstDefer derivVar scope heapDeferred statementss))
       heapStore ident (Deferred var)
       return $ Reference derivVar
inferStatement _ (StmtCommit var identVal)
  = do value <- heapLookup identVal
       commit var value
       return Opaque
inferStatement combine (StmtFixate statement)
  = incScope $
      do deriv  <- inferStatement combine statement
         subst0 <- gets currentSubst
         scope  <- gets currentScope
         let vars = Map.keys $ Map.filter (isDeferFromScope scope) subst0
         uniques <- mapM (const fresh) vars
         sequence_ $ zipWith commit vars (map Deferred uniques)
         subst1 <- gets currentSubst
         let vars1 = Map.keys $ Map.filter (isDeferFromScope scope) subst1
         uniques1 <- mapM (const unique) vars1
         sequence_ $ zipWith commitConcrete vars1 (map Fixed uniques1)
         return deriv
  where
    isDeferFromScope scope (SubstDefer _ scope' _ _) = scope == scope'
    isDeferFromScope _ _                             = False

    commitConcrete var value
      = do subst <- gets currentSubst
           entry <- mapLookup var subst
           case entry of
             SubstValue _ -> return ()
             _            -> commit var value
inferStatement _ (StmtFixpoint ident bindings statement)
  = do modifyHeap (\heap -> takeWithRenaming (reverseBindings bindings) heap `Map.union` heap)
       fmap Transitive $ fixpoint [] $
         do deriv <- inferStatement (tryInferAll Transitive) statement
            modifyHeap (\heap -> takeWithRenaming bindings heap `Map.union` heap)
            return deriv
  where
    fixpoint accum m
      = do heap0  <- gets currentHeap
           deriv  <- m
           heap1  <- gets currentHeap
           v0     <- Map.lookup ident heap0
           v1     <- Map.lookup ident heap1
           if v0 /= v1
            then fixpoint (deriv:accum) m
            else return accum

runInferencer :: Statement -> Rules -> Heap -> Either Err Deriv
runInferencer statement rules heap
  = evalState (runErrorT (inferStatement tryInferOne (StmtFixate statement) >>= composeDeriv)) $
      IState { allRules = rules, uniqueCounter = 1, currentSubst = Map.empty
             , currentHeap = heap, currentScope = 0, deferredDerivs = Map.empty }

-- Stores a fresh value under a given name in the heap. This fresh value is
-- constructed from a unique variable with a conversion function. Examples of
-- this conversion function are deferredValue and fixedValue.
-- This function is intended to be used as "fresh value statement".
relFresh :: (Var -> Value) -> Ident -> I ()
relFresh f name
  = do i <- fresh
       let v = f i
       heapStore name v

-- Load a value from the heap, run a pattern-match function on it which, when
-- succeeds, returns a list of bindings of identifier to value. These bindings
-- will then be added to the heap. If the match fails, execution aborts, but is
-- allowed to continue if there is an alternative executable possible.
-- This function is intended to be used as "pattern match statement".
relMatch :: Typeable v => Ident -> (v -> Maybe [(Ident, Value)]) -> I ()
relMatch ident f
  = do Value _ v <- heapLookup ident
       case f (fromJust $ cast v) of
         Just outputs -> mapM_ (uncurry heapStore) outputs
         Nothing      -> throwErrorRecoverable "relMatch: pattern match failed"


--
-- Example
-- Run 't' to execute.
--

t    = runInferencer stmt rules heap
stmt = StmtScheme (Map.fromList [("g", "gIn"), ("e", "eIn")]) (Map.fromList [("t", "tOut")]) "ExprG"
heap = Map.fromList [("gIn", Value [] (Map.empty :: Map String Ty)), ("eIn", Value [] expr)]
-- - expr  = EApp (ETLam "f" (TAll (-1) TBot $ TArr (TArr TInt (fixedValue (-1))) (TArr TInt (fixedValue (-1)))) $ EApp (EVar "f") (ELam "x" $ EVar "x")) (ELam "x" $ EVar "x")
-- expr = ELet "id" (ELam "x" $ EVar "x") $ EApp (EApp (EVar "id") (EVar "id")) (EVar "id")
-- expr = ELet "id" (ELam "x" $ EVar "x") $ EApp (EApp (EVar "id") (EVar "id")) (EInt 3)
-- expr = ELet "f" (EFix $ ELam "x" $ EVar "x") $ EFix $ ELam "r" $ ELam "n" $ EApp (EVar "r") $ EApp (EApp (EVar "f") (EVar "n")) (EApp (EVar "r") (EInt 3))
expr = EFix $ ELam "r" $ ELam "n" $ EInt 3
-- expr = ELet "f" (EFix $ ELam "x" $ EVar "x") $ ELam "r" $ EApp (EVar "r") (EApp (EVar "f") (EVar "r"))
-- expr = ELet "f" (EFix $ ELam "x" $ EVar "x") $ EApp (EVar "f") (EInt 3)
-- expr = ELet "id" (ELam "x" $ EVar "x") $ EApp (EVar "id") (EInt 3)
rules = Set.fromList
  [ Rule "e.int"
     [ StmtExec $ relMatch "e" $ \e ->
         case e of
           EInt _ -> Just []
           _      -> Nothing
     , StmtExec $ storeUD "t" TInt
     , StmtExec $ storeF "p" emptyPrefix
     ]
     (StmtScheme (Map.fromList [("g","g"),("e","e")]) (Map.fromList [("t", "t"), ("p", "p")]) "Expr")
  , Rule "e.var"
     [ StmtExec $ relMatch "e" $ \e ->
                     case e of
                       EVar x -> Just [("x", Value [] x)]
                       _      -> Nothing
     , StmtScheme (Map.fromList [("g","g"),("x","x")]) (Map.singleton "t" "t") "LookupGam"
     , StmtExec $ storeF "p" emptyPrefix
     ]
     (StmtScheme (Map.fromList [("g","g"),("e","e")]) (Map.fromList [("t", "t"), ("p", "p")]) "Expr")
  , Rule "e.app"
     [ StmtExec $ relMatch "e" $ \e ->
                   case e of
                     EApp f a -> Just [("f", Value [] f), ("a", Value [] a)]
                     _        -> Nothing
     , StmtScheme (Map.fromList [("g","g"), ("e","f")]) (Map.fromList [("t", "tF"), ("p", "pF")]) "ExprI"
     , StmtScheme (Map.fromList [("g","g"), ("e","a")]) (Map.fromList [("t", "tA"), ("p", "pA")]) "ExprI"
     , StmtScheme Map.empty (Map.singleton "t" "tRes") "FreshTy"
     , StmtExec $ do tA   <- loadD "tA"   :: I Ty
                     tRes <- loadD "tRes" :: I Ty
                     storeUD "tF'" (TArr tA tRes)
     , StmtUnify "tF" "tF'"
     , StmtExec $ prefixUnion "pF" "pA" "pRes"
     ]
     (StmtScheme (Map.fromList [("g","g"),("e","e")]) (Map.fromList [("t", "tRes"), ("p", "pRes")]) "Expr")
  , Rule "e.lam"
     [ StmtExec $ relMatch "e" $ \e ->
                     case e of
                       ELam x b -> Just [("x", Value [] x), ("b", Value [] b)]
                       _        -> Nothing
     , StmtScheme Map.empty (Map.singleton "t" "tX") "FreshTy"
     , StmtScheme (Map.fromList [("g","g"),("x","x"),("t","tX")]) (Map.singleton "g'" "gB") "ExtendGam"
     , StmtScheme (Map.fromList [("g","gB"), ("e","b")]) (Map.fromList [("t", "tB"), ("p", "p")]) "Expr"
     , StmtExec $ do tB <- loadD "tB" :: I Ty
                     tX <- loadD "tX" :: I Ty
                     storeUD "tRes" (TArr tX tB)
     ]
     (StmtScheme (Map.fromList [("g","g"),("e","e")]) (Map.fromList [("t", "tRes"), ("p", "p")]) "Expr")
  , Rule "e.tlam"
      [ StmtExec $ relMatch "e" $ \e ->
                     case e of
                       ETLam x t b -> Just [("x", Value [] x), ("tX", tyMkValue t), ("b", Value [] b)]
                       _           -> Nothing
     , StmtScheme (Map.fromList [("g","g"),("x","x"),("t","tX")]) (Map.singleton "g'" "gB") "ExtendGam"
     , StmtScheme (Map.fromList [("g","gB"), ("e","b")]) (Map.fromList [("t", "tB"), ("p", "p")]) "Expr"
     , StmtExec $ do tB <- loadD "tB" :: I Ty
                     tX <- loadD "tX" :: I Ty
                     storeUD "tRes" (TArr tX tB)
     ]
     (StmtScheme (Map.fromList [("g","g"),("e","e")]) (Map.fromList [("t", "tRes"), ("p", "p")]) "Expr")
  , Rule "e.let"
      [ StmtExec $ relMatch "eLet" $ \eLet ->
                      case eLet of
                        ELet x b e -> Just [("x", Value [] x), ("b", Value [] b), ("e", Value [] e)]
                        _          -> Nothing
      , StmtScheme (Map.fromList [("g","g"),("e","b")]) (Map.fromList [("t", "tX"), ("p", "pX")]) "ExprG"
      , StmtScheme (Map.fromList [("g","g"),("x","x"),("t","tX")]) (Map.singleton "g'" "g'") "ExtendGam"
      , StmtScheme (Map.fromList [("g","g'"),("e","e")]) (Map.fromList [("t", "t"), ("p", "pE")]) "Expr"
      , StmtExec $ prefixUnion "pX" "pE" "p"
      ]
      (StmtScheme (Map.fromList [("g","g"),("e","eLet")]) (Map.fromList [("t", "t"), ("p", "p")]) "Expr")
  , Rule "e.fix"
      [ StmtExec $ relMatch "e" $ \e ->
                      case e of
                        EFix f -> Just [("f", Value [] f)]
                        _      -> Nothing
      , StmtScheme (Map.fromList [("g","g"),("e","f")]) (Map.fromList [("t", "tF"), ("p", "p")]) "ExprI"
      , StmtScheme Map.empty (Map.singleton "t" "tFresh") "FreshTy"
      , StmtExec $ do t <- loadD "tFresh" :: I Ty
                      storeUD "tF'" (TArr t t)
      , StmtUnify "tF'" "tF"
      ]
      (StmtScheme (Map.fromList [("g","g"),("e","e")]) (Map.fromList [("t", "tFresh"), ("p", "p")]) "Expr")

  -- generalization
  , Rule "e.g.gen"
      [ StmtFixate $ StmtScheme (Map.fromList [("g","g"),("e","e")]) (Map.fromList [("t", "t"), ("p", "p")]) "Expr"
      , StmtExec $ do t <- loadD "t" :: I Ty
                      g <- loadF "g" :: I (Map String Ty)
                      let freevars = ftv t \\ ftv g
                      storeF "as" freevars
      , StmtExec $ do t      <- loadD "t"  :: I Ty
                      a      <- loadF "as" :: I [Var]
                      bndMap <- finalizePrefix "p"
                      storeUD "t'" $ foldr (\v r -> TAll v (Map.findWithDefault TBot v bndMap) r) t a
                      storeF "pOut" $ MapConcrete $ foldr Map.delete bndMap a
      ]
      (StmtScheme (Map.fromList [("g","g"),("e","e")]) (Map.fromList [("t", "t'"), ("p", "pOut")]) "ExprG")

  -- delayed instantiation
  , Rule "e.i.inst"
      [ StmtScheme (Map.fromList [("g","g"),("e","e")]) (Map.fromList [("t", "t1"), ("p", "pE")]) "ExprG"
      , StmtDefer "t2" ["pI"]
          [[ StmtScheme (Map.fromList [("t1","t1"),("t2","t2")]) (Map.singleton "p" "pI") "Inst" ]]
      , StmtExec $ prefixUnion "pE" "pI" "p"
      ]
      (StmtScheme (Map.fromList [("g","g"),("e","e")]) (Map.fromList [("t", "t2"), ("p", "p")]) "ExprI")

  -- Rule I-Bottom.
  , Rule "i.bot"
      [ StmtExec $ relMatch "t1" $ \t1 ->
                      case t1 of
                        TBot -> Just []
                        _    -> Nothing
      , StmtExec $ storeF "p" emptyPrefix
      ]
      (StmtScheme (Map.fromList [("t1","t1"),("t2","t2")]) (Map.singleton "p" "p") "Inst")

  -- Rule I-Hyp
{-
  , Rule "i.hyp"
      [ StmtExec $ relMatch "t1" $ \t1 ->
                      case t1 of
                        TBot -> Nothing
                        _    -> Just []
      , StmtExec $ relMatch "t2" $ \t2 ->
                      case t2 of
                        TVar _ v -> Just [("v", Value [] v)]
                        _        -> Nothing
      , StmtExec $ do v  <- loadF "v" :: I Var
                      t1 <- loadD "t1" :: I Ty
                      storeF "p"   $ singletonPrefix v t1
      ]
      (StmtScheme (Map.fromList [("t1","t1"),("t2","t2")]) (Map.singleton "p" "p") "Inst") -}

  -- Dispatcher (using rewriting)
  , Rule "i.dispatcher"
      [ -- prevent overlap with the I-Bottom and I-Hyp rule
        StmtExec $ do relMatch "tInit" $ \tInit ->
                         case tInit of
                           TBot -> Nothing
                           _    -> Just []
{-      , StmtExec $ do relMatch "tResult" $ \tResult ->
                         case tResult of
                           TVar _ _ -> Nothing
                           _        -> Just []   -}
      , StmtExec $ do tInit   <- loadD "tInit"   :: I Ty
                      tResult <- loadD "tResult" :: I Ty
                      instInfo <- fmap (addBoundsInfo (bounds tResult)) $ partialUnify (body tInit) (body tResult)
                      subst <- gets currentSubst
                      storeF "iSubst" instInfo
                      storeF "iMiss" (Map.difference (bounds tResult) (bounds tInit))
                      storeF "iOrder" (boundsOrder tResult)
                      storeUD "t" tInit
                      storeF "p" emptyPrefix
      , StmtScheme (Map.fromList [("iMiss","iMiss"),("t1","t")]) (Map.singleton "t2" "t") "InstMissing"
      , StmtFixpoint "t" (Map.fromList [("t","t"),("p","p")]) $
          StmtScheme (Map.fromList [("iSubst","iSubst"),("t1","t"),("p1","p")]) (Map.fromList [("t2","t"),("p2","p")]) "InstSubst"
      , StmtFixpoint "t" (Map.singleton "t" "t") $
          StmtScheme (Map.fromList [("iOrder","iOrder"),("t1","t")]) (Map.singleton "t2" "t") "InstReorder"
      , StmtUnify "t" "tResult"
      ]
      (StmtScheme (Map.fromList [("t1","tInit"),("t2","tResult")]) (Map.singleton "p" "p") "Inst")

  -- I-context reduction
  , Rule "i.context"
      [ StmtExec $ do instInfo <- loadF "iSubst" :: I (Map Var (Either Ty Ty))
                      relMatch "t1" $ \ t1 ->
                         case t1 of
                           TAll v b t -> case Map.lookup v instInfo of
                                           Just (Left b')  | b /= b' -> Just [("v", Value [] v),("b", tyMkValue b),("b'", tyMkValue b'), ("t", tyMkValue t)]
                                           Just (Right b') | b /= b' -> Just [("v", Value [] v),("b", tyMkValue b),("b'", tyMkValue b'), ("t", tyMkValue t)]
                                           _                         -> Nothing
                           _          -> Nothing
      , StmtScheme (Map.fromList [("iSubst","iSubst"),("t1","b"),("t2","b'")]) (Map.singleton "p" "p") "Inst"
      , StmtExec $ do v  <- loadF "v"  :: I Var
                      b' <- loadD "b'" :: I Ty
                      t  <- loadD "t"  :: I Ty
                      let t2 =  TAll v b' t
                      storeUD "t2" t2
                      prefixUnion "p1" "p" "p2"
      ]
      (StmtScheme (Map.fromList [("iSubst","iSubst"),("t1","t1"),("p1","p1")]) (Map.fromList [("t2","t2"),("p2","p2")]) "InstSubst")

  -- I-prefix step-over
  , Rule "i.prefix"
      [ StmtExec $ do instInfo <- loadF "iSubst" :: I (Map Var (Either Ty Ty))
                      relMatch "t1" $ \t1 ->
                        case t1 of
                          TAll v b t -> case Map.lookup v instInfo of
                                          Just (Left b') | b == b' -> Just [("v", Value [] v),("b", tyMkValue b),("t", tyMkValue t)]
                                          Nothing                  -> Just [("v", Value [] v),("b", tyMkValue b),("t", tyMkValue t)]
                                          _                        -> Nothing
                          _          -> Nothing
      , StmtExec $ storeF "p" emptyPrefix
      , StmtScheme (Map.fromList [("iSubst","iSubst"),("t1","t"),("p1","p")]) (Map.fromList [("t2","t'"),("p2","p")]) "InstSubst"
      , StmtExec $ do v  <- loadF "v"  :: I Var
                      b  <- loadD "b"  :: I Ty
                      t' <- loadD "t'" :: I Ty
                      storeUD "t2" $ TAll v b t'
                      prefixDelete v b "p" "p'"
                      prefixUnion "p1" "p'" "p2"
      ]
      (StmtScheme (Map.fromList [("iSubst","iSubst"),("t1","t1"),("p1","p1")]) (Map.fromList [("t2","t2"),("p2","p2")]) "InstSubst")

  -- I-Subst reduction
  , Rule "i.subst"
      [ StmtExec $ do instInfo <- loadF "iSubst" :: I (Map Var (Either Ty Ty))
                      relMatch "t1" $ \ t1 ->
                         case t1 of
                           TAll v b t -> case Map.lookup v instInfo of
                                           Just (Right b') | b == b' -> Just [("v", Value [] v),("b", tyMkValue b),("t", tyMkValue t)]
                                           _                         -> Nothing
                           _          -> Nothing
      , StmtExec $ do v <- loadF "v" :: I Var
                      t <- loadD "t" :: I Ty
                      b <- loadD "b" :: I Ty
                      if not (isRho b) && v `elem` etv t
                       then throwErrorAbort "rel i.subst: variable in the exposed type variables."
                       else return ()
      , StmtExec $ do v <- loadF "v" :: I Var
                      b <- loadD "b" :: I Ty
                      t <- loadD "t" :: I Ty
                      let t2 = appSubst (Map.singleton v $ SubstValue $ tyMkValue b) t
                      storeUD "t2" $ t2
      ]
      (StmtScheme (Map.fromList [("iSubst","iSubst"),("t1","t1"),("p1","p")]) (Map.fromList [("t2","t2"),("p2","p")]) "InstSubst")

  -- I-Equiv terminator
  , Rule "i.equiv"
      [ StmtExec $ do relMatch "t" $ \ t ->
                         case t of
                           TAll _ _ _ -> Nothing
                           _          -> Just []
      ]
      (StmtScheme (Map.fromList [("iSubst","iSubst"),("t1","t"),("p1","p")]) (Map.fromList [("t2","t"),("p2","p")]) "InstSubst")

  -- instantiation (insert missing quantifiers)
  , Rule "i.insert.missing"
      [ StmtExec $ do ms <- loadF "iMiss" :: I (Map Var Ty)
                      t1 <- loadD "t1"    :: I Ty
                      let t2 = foldr (\(v,b) r -> TAll v b r) t1 $ Map.toList ms
                      storeUD "t2" t2
      ]
      (StmtScheme (Map.fromList [("iMiss","iMiss"),("t1","t1")]) (Map.singleton "t2" "t2") "InstMissing")

  -- instantiation (reorder universal quantifiers)
  , Rule "i.reorder.swap"
      [ StmtExec $ relMatch "t1" $ \t1 ->
                      case t1 of
                        TAll v1 b1 (TAll v2 b2 t) -> Just [("v1", Value [] v1), ("v2", Value [] v2), ("b1", tyMkValue b1), ("b2", tyMkValue b2), ("t", tyMkValue t)]
                        _                         -> Nothing
      , StmtExec $ do oMap <- loadF "iOrder" :: I (Map Var Int)
                      v1   <- loadF "v1"     :: I Var
                      v2   <- loadF "v2"     :: I Var
                      n1   <- mapLookup v1 oMap
                      n2   <- mapLookup v2 oMap
                      continueIff (n1 > n2)
      , StmtExec $ do v1   <- loadF "v1"     :: I Var
                      b1   <- loadD "b1"     :: I Ty
                      v2   <- loadF "v2"     :: I Var
                      b2   <- loadD "b2"     :: I Ty
                      t    <- loadD "t"      :: I Ty
                      let t2 = TAll v2 b2 $ TAll v1 b1 t
                      storeUD "t2" t2
      ]
      (StmtScheme (Map.fromList [("iOrder","iOrder"),("t1","t1")]) (Map.singleton "t2" "t2") "InstReorder")
  , Rule "i.reorder.step"
      [ StmtExec $ relMatch "t1" $ \t1 ->
                      case t1 of
                        TAll v1 b1 (TAll v2 b2 t) -> Just [("v1", Value [] v1), ("v2", Value [] v2), ("b1", tyMkValue b1), ("b2", tyMkValue b2), ("t", tyMkValue t)]
                        _                         -> Nothing
      , StmtExec $ do oMap <- loadF "iOrder" :: I (Map Var Int)
                      v1   <- loadF "v1"     :: I Var
                      v2   <- loadF "v2"     :: I Var
                      n1   <- mapLookup v1 oMap
                      n2   <- mapLookup v2 oMap
                      continueIff (n1 <= n2)
      , StmtExec $ do v2 <- loadF "v2" :: I Var
                      b2 <- loadD "b2" :: I Ty
                      t  <- loadD "t"  :: I Ty
                      storeUD "t1b" (TAll v2 b2 t)
      , StmtScheme (Map.fromList [("iOrder","iOrder"),("t1","t1b")]) (Map.singleton "t2" "t2b") "InstReorder"
      , StmtExec $ do t2b <- loadD "t2b" :: I Ty
                      v1  <- loadF "v1"  :: I Var
                      b1  <- loadD "b1"  :: I Ty
                      storeUD "t2" (TAll v1 b1 t2b)
      ]
      (StmtScheme (Map.fromList [("iOrder","iOrder"),("t1","t1")]) (Map.singleton "t2" "t2") "InstReorder")

  -- auxilery relations
  , Rule "fresh.ty"
      [StmtExec $ relFresh (tyMkValue . deferValue) "t"]
      (StmtScheme Map.empty (Map.singleton "t" "t") "FreshTy")
  , Rule "lookup.gam"
      [ StmtExec $ do x <- loadF "x" :: I String
                      g <- loadF "g" :: I (Map String Ty)
                      t <- mapLookup x g
                      storeUD "t" t
      ]
      (StmtScheme (Map.fromList [("g","g"),("x","x")]) (Map.singleton "t" "t") "LookupGam")
  , Rule "extend.gam"
      [ StmtExec $ do x <- loadF "x" :: I String
                      t <- loadD "t" :: I Ty
                      g <- loadF "g" :: I (Map String Ty)
                      storeF "g'" (Map.insert x t g)
      ]
      (StmtScheme (Map.fromList [("g","g"),("x","x"),("t","t")]) (Map.singleton "g'" "g'") "ExtendGam")
  ]


--
-- Example support code.
--

-- Syntax for expressions with a possible explicit type for a lambda parameter.
data Expr :: * where
  EInt  :: Int -> Expr
  EVar  :: String -> Expr
  EApp  :: Expr -> Expr -> Expr
  ELam  :: String -> Expr -> Expr
  ETLam :: String -> Ty -> Expr -> Expr
  ELet  :: String -> Expr -> Expr -> Expr
  EFix  :: Expr -> Expr
  deriving (Eq, Ord, Typeable)

-- HML types. Universal quantifiers have a bound. Variables come in two
-- flavours: fixed variables and deferred variabels. Deferred variables
-- represent delayed instantiation whereas fixed variables represent
-- polymorphism.
data Ty :: * where
  TInt :: Ty
  TArr :: Ty -> Ty -> Ty
  TAll :: Var -> Ty -> Ty -> Ty
  TBot :: Ty
  TVar :: Category -> Var -> Ty
  deriving (Eq, Ord, Typeable)

-- The two flavours of type variables.
data Category :: * where
  CatDeferred :: Category
  CatFixed    :: Category
  deriving (Eq, Ord, Show, Typeable)

instance Show Expr where
  show (EInt i)      = show i
  show (EVar x)      = x
  show (EApp f a)    = "(" ++ show f ++ " " ++ show a ++ ")"
  show (ELam n e)    = "(\\" ++ n ++ " -> " ++ show e ++ ")"
  show (ETLam n t e) = "(\\" ++ n ++ " :: " ++ show t ++ " -> " ++ show e ++ ")"
  show (ELet n e b)  = "let " ++ n ++ " = " ++ show e ++ " in " ++ show b
  show (EFix e)      = "fix " ++ show e

instance Show Ty where
  show (TVar CatFixed v)    = "#" ++ show v
  show (TVar CatDeferred v) = "%" ++ show v
  show TInt                 = "Int"
  show (TArr f a)           = "(" ++ show f ++ " -> " ++ show a ++ ")"
  show (TAll v b t)         = "(forall #" ++ show v ++ " >= " ++ "(" ++ show b ++ ") . " ++ show t ++ ")"
  show TBot                 = "_|_"

instance Container (Map k Ty) where
  appSubst subst = Map.map (appSubst subst)

instance Container (Map k (Either Var Ty)) where
  appSubst subst = Map.map (either Left (Right . appSubst subst))

instance Container (Map k (Either Ty Ty)) where
  appSubst subst = Map.map (either (Left . appSubst subst) (Right . appSubst subst))

instance Container (Map Var Int)
instance Container Expr
instance Container Char

instance Container Ty where
  appSubst subst (TArr a r)
    = TArr (appSubst subst a) (appSubst subst r)
  appSubst subst (TAll v b t)
    = TAll v (appSubst subst b) (appSubst subst t)
  appSubst subst t@(TVar _ v)
    = case Map.lookup v subst of
        Just (SubstValue (Value _ v)) ->
          case cast v of
            Just r  -> r
            Nothing -> error "appSubst: invalid type (cast failed)"
        Just (SubstValue (Deferred i)) -> deferValue i
        Just (SubstValue (Fixed i))    -> fixedValue i
        _ -> t
  appSubst _ t = t

instance Deferrable Ty where
  deferVars (TArr a r)           = deferVars a ++ deferVars r
  deferVars (TAll _ b t)         = deferVars b ++ deferVars t
  deferVars (TVar CatDeferred v) = [v]
  deferVars _                    = []
  deferValue                     = TVar CatDeferred
  fixedValue                     = TVar CatFixed

-- Structural equality on types. The cases for type variables are handled
-- generically by the function "equal".
instance Unifyable Ty where
  unify (TArr a1 r1) (TArr a2 r2)
                         = equalPairwise a1 a2 r1 r2
  unify (TAll v1 b1 t1) (TAll v2 b2 t2)
    | v1 == v2           = equalPairwise b1 b2 t1 t2
  unify TInt TInt        = return ()
  unify TBot TBot        = return ()
  unify (TVar cat var) t = unifyVar var cat t
  unify t (TVar cat var) = unifyVar var cat t
  unify t1 t2            = throwErrorAbort ("types cannot be unified: " ++ show t1 ++ "  with  " ++ show t2)

instance Unifyable (Either Var Ty) where
  unify (Left v1)  (Left v2) | v1 == v2 = return ()
  unify (Right t1) (Right t2)           = unify t1 t2
  unify _          _                    = throwErrorAbort "unify: incompatible either values."

-- unification of variables is almost completely handled by the generic equality
-- function.
unifyVar :: Var -> Category -> Ty -> I ()
unifyVar var1 cat1 (TVar cat2 var2)  = equal (valFromCat cat1 var1) (valFromCat cat2 var2)
unifyVar var _ t
  | var `elem` deferVars t           = throwErrorAbort "unifyVar: occur check failure: variable exists in its replacement"
  | otherwise                        = commit var (tyMkValue t)

-- Produces an abstract value from the category. I.e. a deferred or fixed
-- abstract value depending on the category.
valFromCat :: Category -> Var -> Value
valFromCat CatDeferred = Deferred
valFromCat CatFixed    = Fixed

-- Converts a type into an abstract value. A type has the capabilities to be
-- deferred and unified.
tyMkValue :: Ty -> Value
tyMkValue = Value [PropDeferrable,PropUnifyable]

-- Overloaded function to obtain free type variables from types and other
-- datastructures storing types. The order of the type variables in the result
-- list is undefined. The result list should not contain duplicates.
class Ftv a where
  ftv :: a -> [Var]

-- The Ftv instance on HML types is actually the "ftv" function when restricted
-- to types where each bound variable is actually mentioned in the body of the
-- type.
instance Ftv Ty where
  ftv (TArr a r)         = nub (ftv a ++ ftv r)
  ftv (TAll v b t)       = nub (ftv b ++ (ftv t \\ [v]))
  ftv (TVar CatFixed v)  = [v]
  ftv _                  = []

instance Ftv (Map String Ty) where
  ftv = nub . concat . Map.elems . Map.map ftv

-- returns if a type is a "rho" type
isRho (TAll _ _ _) = False
isRho _            = True

-- "etv" function on HML types
etv :: Ty -> [Var]
etv (TAll v b t)       = nub (ftv b ++ (ftv t \\ [v]))
etv (TVar CatFixed v)  = [v]
etv _                  = []

-- Returns a list of the outermost bound type variables of the type, in the
-- order of appearance.
btv :: Ty -> [Var]
btv (TAll v _ t) = v : btv t
btv _            = []

-- Returns the bounds of a type.
bounds :: Ty -> Map Var Ty
bounds (TAll v b t) = Map.insert v b (bounds t)
bounds _            = Map.empty

-- Returns the type without the outermost quantifiers.
body :: Ty -> Ty
body (TAll _ _ t) = body t
body t            = t

-- Gives for each variable that is bound topmost, the number of quantifiers
-- occurring to its left.
boundsOrder :: Ty -> Map Var Int
boundsOrder = order 0
  where order n (TAll v _ t) = Map.insert v n $ order (n+1) t
        order _ _            = Map.empty

-- Obtain a map that tells for each topmost bound type variable to what type
-- it is instantiated (or to which variable). There may be parts of the types
-- that are still unknown, or a type variable can occur more than one time. In
-- those cases, we the duplicated are unified.
partialUnify :: Ty -> Ty -> I (Map Var (Either Var Ty))
partialUnify (TAll v1 _ t1) (TAll v2 _ t2)
  | v1 == v2 = partialUnify t1 t2
partialUnify (TArr a1 r1) (TArr a2 r2)
  = do m1    <- partialUnify a1 a2
       subst <- gets currentSubst
       m2    <- partialUnify (appSubst subst r1) (appSubst subst r2)
       let conflicts = Map.elems $ Map.intersectionWith (,) m1 m2
       mapM_ (uncurry unify) conflicts
       subst' <- gets currentSubst
       return $ appSubst subst' $ Map.union m1 m2
partialUnify (TVar cat var) t           = partialUnifyVar False var cat t  -- only for a variable occurring on the left (varables on the right are kept variable unless we know more about it at some point when merging maps)
-- partialUnify t (TVar cat var) | isRho t = partialUnifyVar True var cat t  -- type on the left, but unkown thing on the right: now what?
partialUnify _ _                        = return Map.empty

-- All cases for variables.
partialUnifyVar :: Bool -> Var -> Category -> Ty -> I (Map Var (Either Var Ty))
partialUnifyVar _ var1 CatDeferred (TVar CatDeferred var2)
  = do equal (Deferred var1) (Deferred var2)
       return Map.empty
partialUnifyVar invertedDirection var1 CatDeferred t
  = do commit var1 (tyMkValue t)
       subst <- gets currentSubst
       let flp = if invertedDirection then flip else id
       flp partialUnify (appSubst subst $ deferValue var1) (appSubst subst t)
partialUnifyVar _ var CatFixed (TVar CatFixed var')
  = return $ Map.singleton var (Left var')
partialUnifyVar _ var CatFixed t
  = return $ Map.singleton var (Right t)

-- In the map returned by "partialUnify", some variables are instantiated to
-- other variables. In that case we now replace it with that bound.
addBoundsInfo :: Map Var Ty -> Map Var (Either Var Ty) -> Map Var (Either Ty Ty)
addBoundsInfo bndInfo = Map.map f
  where f (Left v)  = Left $ Map.findWithDefault TBot v bndInfo
        f (Right t) = Right t


-- This DeferrableMap represents a map from variable to type, with one special
-- property: the contents do not need to be defined until the moment it is
-- finialized. The unions and deletions are performed lazily, and a policy for
-- duplicate entries needs to be specified.
data DeferrableMap k v
  = MapDeferred Value
  | MapUnion    (Merger v) (DeferrableMap k v) (DeferrableMap k v)
  | MapDelete   (Merger v) k v (DeferrableMap k v)
  | MapConcrete (Map k v)
  deriving (Eq, Typeable)

instance Show (DeferrableMap k v) where
  show _ = "<prefix>"

-- Merge policy for duplicate entries.
newtype Merger v = Merger (v -> v -> I ())
  deriving Typeable

instance Show (Merger k) where
  show _ = "<<merge function>>"

instance Eq (Merger k) where
  _ == _ = True

-- A merge function that only succeeds when unioning maps if the duplicate
-- entries can be unified.
mergeEqual :: Unifyable v => Merger v
mergeEqual = Merger unify

-- Flattens a deferrable map, thus performing all unions and obtaining the
-- values of the deferred maps. For duplicate values, the merging policy stored
-- in the Union-constructor is used. It is the task of the merging policy to
-- update the substitution in such a way that we only need to keep one of the
-- two duplicate values (we keep the left one). For example, by unifying the
-- two values.
finalizeMap :: (Ord k, Unifyable v, Typeable k, Typeable v, Container (Map k v)) => DeferrableMap k v -> I (DeferrableMap k v)
finalizeMap (MapDeferred v)
  = do subst <- gets currentSubst
       case appSubst subst v of
         Value _ m ->
           case cast m of
             Just mp -> finalizeMap mp
             Nothing -> throwErrorAbort "finalizeMap: failed to cast value to a map."
         _         -> throwErrorAbort ("finalizeMap: the map is not fully defined yet: " ++ show v)
finalizeMap (MapUnion (Merger sem) p1 p2)
  = do MapConcrete mp1 <- finalizeMap p1
       MapConcrete mp2 <- finalizeMap p2
       sequence_ $ Map.elems $ Map.intersectionWith sem mp1 mp2     -- apply the merger on duplicate elements
       subst <- gets currentSubst                                   -- merging may result in an updated substitution
       return $ MapConcrete $ appSubst subst $ Map.union mp1 mp2
finalizeMap (MapDelete (Merger sem) k v m)
  = do MapConcrete mp <- finalizeMap m
       case Map.lookup k mp of
         Just v' -> sem v v'
         Nothing -> return ()
       return $ MapConcrete $ Map.delete k mp
finalizeMap p = return p

instance (Container (Map k v), Container v) => Container (DeferrableMap k v) where
  appSubst subst (MapDeferred v)       = MapDeferred (appSubst subst v)
  appSubst subst (MapUnion m l r)      = MapUnion m (appSubst subst l) (appSubst subst r)
  appSubst subst (MapDelete sem k v r) = MapDelete sem k (appSubst subst v) (appSubst subst r)
  appSubst subst (MapConcrete mp)      = MapConcrete $ appSubst subst mp


-- Representation of the Prefix as deferrable map
type Prefix = DeferrableMap Var Ty

-- Takes the union of two named prefixes and stores the result as the third
-- name. The values of the two input prefixes need to be in the heap, but may
-- be a "Deferred" value (with a substitution to Undefined).
prefixUnion :: Ident -> Ident -> Ident -> I ()
prefixUnion identA identB identC
  = do v1 <- heapLookup identA
       v2 <- heapLookup identB
       let m :: Prefix
           m = MapUnion mergeEqual (MapDeferred v1) (MapDeferred v2)
       storeF identC m

prefixDelete :: Var -> Ty -> Ident -> Ident -> I ()
prefixDelete v t identP identP'
  = do p <- heapLookup identP
       let m :: Prefix
           m = MapDelete mergeEqual v t (MapDeferred p)
       storeF identP' m

-- Collects and combines the individual pieces of the prefix and returns a
-- normal map that can be used to lookup values.
finalizePrefix :: Ident -> I (Map Var Ty)
finalizePrefix ident
  = do mp <- loadF ident :: I Prefix
       mp'@(MapConcrete m) <- finalizeMap mp
       return m

emptyPrefix :: Prefix
emptyPrefix = MapConcrete Map.empty

singletonPrefix :: Var -> Ty -> Prefix
singletonPrefix v t = MapConcrete $ Map.singleton v t

