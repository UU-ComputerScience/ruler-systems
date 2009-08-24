{-# OPTIONS_GHC -fglasgow-exts #-}
module Main where

import Scanner
import Parser
import RulerExpr
import Externals
import Util
import Control.Monad.RWS.Strict
import Control.Monad.Error
import Data.Map(Map)
import qualified Data.Map as Map
import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet(IntSet)
import qualified Data.IntSet as IntSet
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable
import Data.Sequence(Seq)
import qualified Data.Sequence as Seq
import Data.List
import UU.Scanner.Position
import Opts
import System.IO
import Data.Either
import Data.Maybe


--
-- Evaluation
--

evalStmtTop :: Set Pos -> Stmt -> I Bool
evalStmtTop lbls stmt
  = do withDebugLevel 1 $ message ("exec " ++ show (stmtPos stmt))
       evalStmt' lbls stmt `catchError` extendError False ("statement at " ++ show (stmtPos stmt))

evalStmtSeq :: Stmt -> I ()
evalStmtSeq stmt
  = do b <- evalStmt stmt
       if b
        then abort ("sequential statement at " ++ show (stmtPos stmt) ++ " may not be rescheduled")
        else return ()

evalStmt :: Stmt -> I Bool
evalStmt stmt
  = evalStmt' Set.empty stmt `catchError` extendError False ("statement at " ++ show (stmtPos stmt))

-- returns True if the statements should be rescheduled, False otherwise.
evalStmt' :: Set Pos -> Stmt -> I Bool
evalStmt' _ (Stmt_Inst pos expr ident)
  = do vInst <- lookupIdent ident  -- value mapped to derivation to instantiate, must be unassigned
       assert isGuess vInst ("Instantiation to identifier " ++ show ident ++ " is not possible, because it is already assigned.")
       let (ValueGuess uid) = vInst
       v1 <- evalExpr expr
       v1' <- expand v1
       let closure = unwrapThunk v1'
       case closure of
         ValueClosure lvl lbls alts relOrders ->
           do gBranch <- fresh
              gOutcome <- fresh
              let ids        = nub $ collectAlts identsParam alts  -- idents of derivation (concat all param sets)
                  parms      = collectAlts id alts                 -- parameters of derivation (concat all individual params)
                  relOrders' = [visitIdentMain] : map (visitIdentMain :) relOrders
                  order      = totalOrder relOrders'
                  inps       = visitInputs parms (head order)  -- inputs of the first visit
              withDebugLevel 3 $ message ("determined order " ++ show order ++ " from relative orders " ++ show relOrders')
              thunkBindings  <- freshBindings ids  -- fresh bindings of the (entire) derivation
              markBindingsInitialized inps thunkBindings  -- mark the inputs for the first visit as initialized
              let val = ValueThunk uid closure lvl lbls order parms thunkBindings gBranch gOutcome
              unify val vInst
         ValueExternClosure lvl nm params ->
           do outerEnv <- freshBindings (identsParam params)
              markBindingsInitialized (inputs params) outerEnv
              establishedFlag <- fresh
              gOutcome        <- fresh
              let val = ValueExternThunk uid lvl nm params outerEnv establishedFlag gOutcome
              unify val vInst
         _ -> abort ("not a closure: " ++ show v1' ++ " while instantiating as: " ++ show ident ++ "(" ++ show (identPos ident) ++ ")")
       return False
  where
    -- if the "expr" is a thunk, take its closure as the type to instantiate. Since there is always the "this"
    -- variable pointing to the currently executing thunk, it is easy to refer to recursive calls this way.
    unwrapThunk (ValueThunk _ closure _ _ _ _ _ _ _) = closure
    unwrapThunk v = v

    -- collect all the parameters of a ClosureAlt tree structure.
    collectAlts f = concatMap (collectAlt f)
    collectAlt f (ClosureAlt params _ _ _ alts) = f params ++ collectAlts f alts
evalStmt' ctx (Stmt_Establish _ nm mbLvl)
  = do fixateBranch ctx
       v <- lookupIdent nm
       v' <- expand v
       case v' of
         ValueThunk uid closure lvl lbls order params bindings gBranch gOutcome ->
           do -- find out what visit we are to establish
              (visitNm, mbNextNm) <- nextVisit gOutcome order
              let isLast = isNothing mbNextNm
              identsDefined bindings (visitInputs params visitNm)
              mbBranch <- resolveBranch gBranch
              withDebugLevel 3 $ message ("establishing visit: " ++ show visitNm ++ " for " ++ show nm ++ " in order " ++ show order)
              
              -- fixate in case of recursion
              fixateBranch lbls

              -- enter a new "extra roots" scope
              -- these track peer-child derivations that have been announced, but not linked in the visit chain yet
              modify (\ss -> ss { ssExtraRoots = [] : ssExtraRoots ss })

              -- execute the statements belonging to the visit (may fail)
              (eErrRes, os) <- tryExec $
                case mbBranch of -- continue with the execution of an already established alternative?
                  Nothing -> -- not established yet (find a branch)
                    let branches = sortBy (\a1 a2 -> compare (posOf a1) (posOf a2)) $ Map.assocs $ Map.fromListWith (++) [ (nm,[a]) | a@(ClosureAlt _ _ _ (Alt_Alt nm _ _) _) <- closureAlts closure ]
                        isSingleBranch = length branches == 1
                        posOf = identPos . fst

                        evalAlt nm alts
                          = do segment <- createSegment v' alts
                               let fixBranch = assign gBranch (Branch nm segment)
                               local (\is -> is { isCalls = Map.insert lbls fixBranch (isCalls is), isStack = IntSet.insert uid (isStack is) }) $
                                 do when isSingleBranch fixBranch
                                    execUnordered lbls segment
                                    when (not isSingleBranch) fixBranch
                                    withDebugLevel 4 $ checkSegmentExecuted segment
                                    checkOutputsDefined bindings visitNm params
                    in backtrack gBranch (map (uncurry evalAlt) branches)
                  Just (Branch _ segment) -> -- already first visit established
                    local (\is -> is { isStack = IntSet.insert uid (isStack is) }) $
                      do execUnordered lbls segment
                         checkOutputsDefined bindings visitNm params

              -- produce visit status record
              nextVisit <- fresh
              let msgs   = osMessages os
                  childs = case lvl of
                             Level_Hide        -> []
                             Level_Abstract _  -> []
                             _                 -> osDerivations os
                  visit  = Visit visitNm childs msgs nextVisit
              linkVisit gOutcome visit

              -- if finished or error, produce "finished" status record
              case eErrRes of
                Left err              -> linkVisit nextVisit $ Finish $ Failure $ showErrShort err
                Right _ | isLast      -> linkVisit nextVisit $ Finish $ Success
                        | otherwise   -> do -- mark the next inputs as initialized
                                            markBindingsInitialized (visitInputs params (fromJust mbNextNm)) bindings
                                            return ()  -- not finished yet

              -- pop the "extra roots" scope
              case maybe lvl id mbLvl of
                Level_Skip -> let pop (xs : (ys : yss)) = (xs ++ ys) : yss
                              in modify (\ss -> ss { ssExtraRoots = pop (ssExtraRoots ss) })
                _          -> modify (\ss -> ss { ssExtraRoots = tail (ssExtraRoots ss) })

              -- announce the derivation if the first time to establish
              case mbBranch of
                Nothing -> case maybe lvl id mbLvl of  -- and visible
                             Level_Intro      -> established nm v
                             Level_Abstract _ -> established nm v
                             _                -> return ()  -- not visible
                _       -> return ()  -- already announced

              -- announce the results of this subderivation
              case maybe lvl id mbLvl of
                Level_Skip       -> tell os
                Level_Hide       -> tell mempty { osMessages = osMessages os }
                Level_Abstract _ -> tell mempty { osMessages = osMessages os }
                _                -> return ()

              -- at the end of an establish is a good time to run the garbage
              -- collector on the substitution, to clean up intermediate results
              -- that were needed for the computations of the establish.
              either (const $ return ()) (const $ substCollectGarbage False) eErrRes

              either throwError (const $ return $ not isLast) eErrRes  -- reschedule if there are some statements remaining to be executed
         ValueExternThunk _ lvl externNm params bindings gEstablished gOutcome ->
           do assert isGuess gEstablished ("external derivation named " ++ show externNm ++ " has already been established.")  -- an external may only be established once
              assert isGuess gOutcome ("external derivation named " ++ show externNm ++ " has already an outcome.")
              allReady <- identsDefined bindings (inputs params)
              if allReady
               then return ()
               else invalidDeref nm
              (eErrRes, os) <- tryExec $ restrictBindings bindings $ execExternal externNm params
              let result = either (Failure . show) (const Success) eErrRes
              finish <- fresh
              assign finish (Finish result)
              assign gOutcome (Visit visitIdentMain (osDerivations os) (osMessages os) finish)
              assign gEstablished ()
              case lvl of
                Level_Intro      -> established nm v
                Level_Abstract _ -> established nm v
                Level_Hide       -> tell mempty { osMessages = osMessages os }
                _                -> return ()
              either throwError (const $ return False) eErrRes
         _ -> abort ("Not a derivation/external: " ++ show v' ++ " when establishing " ++ explainIdent nm)
  where
    fixateBranch lbls = do callMap <- asks isCalls
                           case Map.lookup lbls callMap of
                             Nothing -> return ()
                             Just m  -> m

    checkOutputsDefined bindings visitNm params
      = do allDefined <- identsDefined bindings (visitOutputs params visitNm)
           if allDefined
            then return ()
            else do bindings' <- expandBindings bindings
                    abort ( "Some of the outputs " ++ intercalate "," (map explainIdent (visitOutputs params visitNm)) ++
                            " for visit " ++ show visitNm ++ " for a derivation named " ++ explainIdent nm ++
                            " remain undefined. Bindings: " ++ explainBindings bindings' )

    checkSegmentExecuted (Segment stmts segs)
      = do mapM_ checkStatementExecuted stmts
           mapM_ checkSegmentExecuted segs

    checkStatementExecuted (ThunkStmt bindings gStmt stmt)
      = do vStmt <- expand gStmt
           when (isGuess vStmt) $
             do reasons <- restrictBindings bindings $ isReady Set.empty stmt
                bindings' <- expandBindings bindings
                message ("A statement " ++ show stmt ++ " was not executed, reasons: " ++ show reasons
                         ++ ", with bindings: " ++ explainBindings bindings')
evalStmt' _ (Stmt_Equiv pos left right)
  = do v1 <- evalExpr left
       v2 <- evalExpr right
       unify v1 v2 `catchError` (extendError True ("failed to unify at " ++ show pos))
       withDebugLevel 3 $ do v1' <- expAll guessLookup v1
                             v2' <- expAll guessLookup v2
                             message ("unified " ++ show v1' ++ " with " ++ show v2')
       return False
evalStmt' _ (Stmt_Bind pos left right)
  = do v1 <- evalExpr left
       v2 <- evalExpr right
       bind v1 v2 `catchError` (extendError True ("failed to bind at " ++ show pos))
       return False
evalStmt' _ (Stmt_Fresh _ nms)
  = do mapM_ (\nm -> do v <- lookupIdent nm
                        withDebugLevel 4 $ message ("fresh: " ++ show v ++ " as " ++ explainIdent nm)
                        markDefined v) nms
       return False
evalStmt' _ (Stmt_Eval _ expr)
  = do evalExpr expr
       return False
evalStmt' _ (Stmt_Nop _)
  = return False


-- executes the segment in a define-before-use order, based on the readyness of
-- statements. Statements in the segment are tried in an inorder-order, skipping
-- over already executed statements.
execUnordered :: Set Pos -> Segment -> I ()
execUnordered lbls root@(Segment rootStmts _)  -- try a speculative execution based on the order of appearance.
  | False -- Set.size lbls == 1    -- TODO: speculative execution seems still a bit erroneous
      = do prevFailed <- gets ssFailedSpeculatives
           if (Set.findMin lbls) `Set.member` prevFailed  -- if speculative execution failed the previous time then forget about it
            then goUnordered
            else goSpeculative rootStmts
  | otherwise = goUnordered
  where
    goUnordered
      = do didOne <- chaseSegment Set.empty root
           if didOne
            then goUnordered  -- repeat chasing until no ready statement anymore
            else return ()

    -- find the next ready statement to execute
    chaseSegment overruled (Segment stmts children)
      = doAny (doFirst (map (chaseStmt overruled) stmts))
              (do stmtRefs <- outputRefs stmts
                  let overruled' = stmtRefs `Set.union` overruled
                  doFirst (map (chaseSegment overruled') children))

    chaseStmt overruled (ThunkStmt bindings gFinished stmt)
          = do vFinished <- expand gFinished
               if isGuess vFinished
                then do stmtRefs <- if Set.null overruled
                                     then return Set.empty
                                     else outputRefs stmt
                        if Set.null (stmtRefs `Set.intersection` overruled)
                         then restrictBindings bindings $
                                do notready <- isReady Set.empty stmt `catchError` extendError False ("while evaluating statements for: " ++ show (Set.toList lbls))
                                   if null notready
                                    then do reschedule <- restrictBindings bindings (evalStmtTop lbls stmt)
                                            when (not reschedule) ( assign gFinished () )
                                            return True
                                    else do withDebugLevel 3 $ message ("Statement at " ++ show (stmtPos stmt) ++ " not ready, due to: " ++ showList notready "")
                                            return False -- this statement is not ready yet
                         else do -- this statement is overruled by above, mark it as executed
                                 assign gFinished ()
                                 return False  -- this statement is overruled by above
                else return False  -- this statement was already performed


    -- execute the statements in the order specified in the list. If this order is "bad", i.e. would result into
    -- a dereference of an uninitialized value, or when a statement needs to be rescheduled, then we fall back to
    -- the normal execution order (defined by readyness)
    goSpeculative [] = return ()
    goSpeculative (stmt@(ThunkStmt bindings _ stmt') : stmts)
      = do prevSubst <- gets ssSubst
           ok <- (restrictBindings bindings (evalStmtTop lbls stmt') >>= return . not) `catchError` (handleRetry prevSubst)
           if ok
            then goSpeculative stmts
            else do withDebugLevel 1 $ message ("switching from speculative execution for derivation with labels " ++ show (Set.toList lbls) ++ " to unordered execution.")
                    modify (\ss -> ss { ssFailedSpeculatives = Set.union lbls (ssFailedSpeculatives ss) })
                    goUnordered

    handleRetry subst (Retry _) = do modify (\ss -> ss { ssSubst = subst })
                                     return False -- swich from speculative to normal execution
    handleRetry _ err           = throwError err  -- error cannot be handled by normal execution either


--
-- Readyness procedure
--
-- Determines a partial order of execution based on a readyness procedure. The idea is that all
-- values referenced to by a statement must be either be a concrete value or an Initialized guess,
-- and that establish requires all inputs to be either a concrete value or a Defined guess.
--
--  inst marks all inputs-guesses to be Initialized
--  fresh marks the introduced guess to be Defined
--  establish marks all output-guesses to be Initialized
--  var requires a value that is either expanded a concrete value, or an Initialized or Defined guess.
--
-- This ensures that inputs of a derivation can be properly assigned to after the "inst" has been
-- performed.
--
-- There is one particular difficulty: Expr_Seq introduces statements (with explicit order). These
-- statements can introduce identifiers. These are ignored and assumed to be Defined.
--

data NotreadyReason
  = IdentNotReady !Ident !(Set Ident)
  | FieldNotReady !Ident !(Set Ident)
  | NotAllInputsReady !Ident !(Set Ident)
  | FieldIdentNotReady !Ident !Ident !(Set Ident)

instance Show NotreadyReason where
  show (IdentNotReady ident ignore) = "identifier " ++ show ident ++ " (" ++ show (identPos ident) ++ ") not ready (ignore list: " ++ show (Set.toList ignore) ++ ")"
  show (FieldNotReady ident ignore) = "field " ++ show ident ++ " (" ++ show (identPos ident) ++ ") not ready (ignore list: " ++ show (Set.toList ignore) ++ ")"
  show (FieldIdentNotReady fld ident ignore) = "identifier " ++ show ident ++ " (" ++ show (identPos ident) ++ ") of field " ++ show fld ++ " (" ++ show (identPos fld) ++ ") not ready (ignore list: " ++ show (Set.toList ignore) ++ ")"
  show (NotAllInputsReady ident ignore) = "some inputs of field " ++ show ident ++ " (" ++ show (identPos ident) ++ ") not ready (ignore list: " ++ show (Set.toList ignore) ++ ")"
  
class IsReady a where
  isReady :: Set Ident -> a -> I [NotreadyReason]

instance IsReady a => IsReady [a] where
  isReady ignore = doConcat . map (isReady ignore)

instance IsReady Stmt where
  isReady ignore (Stmt_Inst _ expr _)      = isReady ignore expr
  isReady ignore (Stmt_Establish _ nm _)
    | nm `Set.member` ignore               = return []
    | otherwise = do v <- lookupIdent nm
                     v' <- expand v
                     case v' of
                       ValueThunk _ _ _ _ order params bindings _ gVisits -> do (visitNm, _) <- nextVisit gVisits order
                                                                                checkDefined bindings (visitInputs params visitNm)
                       ValueExternThunk _ _ _ params bindings _ _         -> checkDefined bindings (inputs params)
                       _                                                  -> return [ FieldNotReady nm ignore ]
                where
                  checkDefined bindings ids = do areDefined <- identsDefined bindings ids
                                                 if areDefined
                                                  then return []
                                                  else return [ NotAllInputsReady nm ignore ]
                  checkNextVisitReady :: [Ident] -> Params -> Map Ident Value -> Value -> I [NotreadyReason]
                  checkNextVisitReady = undefined
  isReady ignore (Stmt_Equiv _ left right) = doConcat [ isReady ignore left, isReady ignore right ]
  isReady ignore (Stmt_Bind _ left right)  = doConcat [ isReady ignore left, isReady ignore right ]
  isReady _ (Stmt_Fresh _ _)               = return []
  isReady ignore (Stmt_Eval _ expr)        = isReady ignore expr
  isReady _ (Stmt_Nop _)                   = return []

instance IsReady Expr where
  isReady _ (Expr_Var Mode_Def _) = return []
  isReady ignore (Expr_Var _ nm)
    | nm `Set.member` ignore = return []
    | otherwise =
        do v <- lookupIdent nm
           isDefined <- isInitializedValue v
           if isDefined
             then return []
             else return [ IdentNotReady nm ignore ]
  isReady ignore (Expr_Field fld nm)
    | fld `Set.member` ignore = return []
    | otherwise =
        do vFld <- lookupIdent fld
           vFld' <- expand vFld
           if isGuess vFld'
            then return [ FieldNotReady fld ignore ]
            else do (isContra, isOutput) <- getFieldInfo fld nm
                    v <- lookupField fld nm
                    b <- isFieldReady isContra isOutput v
                    return (if b then [] else [ FieldIdentNotReady fld nm ignore ])
  isReady ignore (Expr_Seq stmts expr) = let ignore' = Set.fromList (explicitIntroducedIdents stmts) `Set.union` ignore
                                         in doConcat [ isReady ignore' stmts, isReady ignore' expr ]
  isReady ignore (Expr_Merge _ exprs)        = isReady ignore exprs
  isReady ignore (Expr_Inherit _ expr exprs) = doConcat [ isReady ignore expr, isReady ignore exprs ]
  isReady _ _                                = return []

evalExpr :: Expr -> I Value
evalExpr (Expr_Var Mode_Ref nm)      = checkValueInitialized nm $ lookupIdent nm
evalExpr (Expr_Var Mode_Def nm)      = do v <- lookupIdent nm
                                          markDefined v
                                          return v
evalExpr (Expr_Field fld nm)         = do (isContra, isOutput) <- getFieldInfo fld nm
                                          v <- lookupField fld nm
                                          b <- isFieldReady isContra isOutput v
                                          when (not b) (invalidDeref fld)
                                          when (isContra && isOutput) $
                                            markDefined v -- output field of derivation may not be initialized yet. This dereference defines it.
                                                          -- this we do, because it may be assigned to some local variable, on which we want the
                                                          -- "checkInitialized" check to succeed
                                          return v
evalExpr (Expr_Prim _ (PrimVal val)) = mkOpaque val
evalExpr (Expr_Seq stmts expr)
  = do bindings <- foldl evalSeqStmt (return Map.empty) stmts
       scopeBindings bindings (evalExpr expr)
  where
    evalSeqStmt prev stmt
      = do prevBindings <- prev
           bindings <- freshBindings (explicitIntroducedIdents [stmt] \\ Map.keys prevBindings)  -- don't allocate new bindings for already defined ones for this Seq
           let newBindings = bindings `Map.union` prevBindings
           scopeBindings newBindings (evalStmtSeq stmt)
           return newBindings
evalExpr (Expr_Derivation pos (Order_Relative order) params innername lvl alts)
  = do globalBindings <- asks isBindings
       let alts' = map (\alt -> ClosureAlt params innername globalBindings alt []) alts
           vsOmitted = Set.toList (visits params `Set.difference` Set.fromList order) -- visits not in the relative order list
       return $ ValueClosure lvl (Set.singleton pos) alts' [order, filter (/= visitIdentMain) vsOmitted]
evalExpr (Expr_External _ nm params lvl)
  = return $ ValueExternClosure lvl nm params
evalExpr (Expr_Merge _ exprs)
  | length exprs == 0 = abort "empty merge"
  | otherwise =
      do closures <- mapM evalExprAsClosure exprs
         let lbls = Set.unions $ map closureLabels closures
             alts = concatMap closureAlts closures
             lvl  = head (map closureLevelOpt closures)
             ords = concatMap closureOrder closures
         return $ ValueClosure lvl lbls alts ords
evalExpr (Expr_Inherit _ expr exprs)
  | length exprs == 0 = abort "empty inherit"
  | otherwise =
      do closures <- mapM evalExprAsClosure exprs
         closure  <- evalExprAsClosure expr
         let altsMap = Map.fromListWith (++) [ (altName alt, [alt]) | alt <- concatMap closureAlts closures ]
             allClosures = closure : closures
             lbls = Set.unions $ map closureLabels allClosures
             ords = concatMap closureOrder allClosures
         case closure of
           ValueClosure lvl _ alts _ ->
             let alts' = map extendAlt alts
                 extendAlt (ClosureAlt params innername bindings alt@(Alt_Alt nm _ _) children)
                   = let extra = Map.findWithDefault [] nm altsMap
                     in ClosureAlt params innername bindings alt (children ++ extra)
             in return (ValueClosure lvl lbls alts' ords)
           _ -> abort ("not a closure: " ++ show closure)

evalExprAsClosure :: Expr -> I Value
evalExprAsClosure expr
  = do v <- evalExpr expr
       v' <- expand v
       case v' of
         ValueClosure _ _ _ _ -> return v'
         _                    -> abort ("evaluation of expr did not result in a closure: " ++ show v')

-- evalutes the given ruler expression, allowing for the construction of a result through the
-- inspection monad (to access the guess-memory)
evaluate :: (DerivationTree -> IT m a) -> Opts -> Stmts -> Expr -> (Maybe (m a), Maybe String)
evaluate f opts stmts e
  = either (\e -> (Nothing, Just (show e))) (\(a,m) -> (Just a, m)) r
  where
    sysPos = Pos 0 0 "<internal system>"
    mainId = ident "__main"
    rootId = ident "__root"
    argsId = ident "args"
    vRoot  = ValueGuess rootGuess
    startUnique = rootGuess + 1
    e' = Expr_Seq ( externalsToStmts externals ++ stmts ++
                    [ Stmt_Inst sysPos e mainId
                    , Stmt_Equiv sysPos (Expr_Var Mode_Ref mainId) (Expr_Var Mode_Ref rootId)
                    , Stmt_Equiv sysPos (Expr_Field mainId argsId) (Expr_Prim sysPos $ PrimVal $ convertList $ map PS $ programArgs opts)
                    , Stmt_Establish sysPos mainId Nothing
                    ])
                  (Expr_Var Mode_Def mainId)
    pipeline = do (eErrRes,_) <- tryExec (evalExpr e')
                  assertFinished vRoot
                  t <- toDerivation vRoot
                  subst <- gets ssSubst
                  let a = runIT (f t) (ITIn { itSubst = subst, itOpts = opts })
                  case eErrRes of
                    Left err -> return $ (a, Just (show err))
                    Right _  -> return $ (a, Nothing)
    (r, _, _) = runRWS (runErrorT pipeline) inpState shState
    inpState  = InputState { isBindings = Map.singleton rootId (ValueGuess rootGuess), isCalls = Map.empty, isStack = IntSet.empty, isOpts = opts, isBindingsStack = [] }
    shState   = SharedState { ssUnique = startUnique, ssSubst = IntMap.singleton rootGuess (ValuePlaceholder Defined), ssFailedSpeculatives = Set.empty, ssLastSubstSize = 500, ssExtraRoots = [[]] }

-- builds a (partial) derivation tree from the thunk representations in memory
toDerivation :: Value -> I DerivationTree
toDerivation root
  = do g <- nextUnique
       to g root
  where
    to g val = do v <- expand val
                  to1 g v `catchError` (extendError False ("in toDerivation on " ++ show v))

    to1 g (ValueThunk _ _ lvl lbls _ params bindings branch outcome)
      = do (visits,status) <- getOutcome outcome
           (Branch nmBranch _) <- tryResolve branch (Branch (ident "_undetermined_") (error "toDerivation: do not touch the segment"))
           let inps = inputs params
               title = case lvl of
                         Level_Abstract nm -> nm
                         _                 -> nmBranch
               context = showLblsShort lbls
               (mIn,mOut) = Map.partitionWithKey (\k _ -> k `elem` inps) bindings
           return (DTNode g title context (Map.assocs mIn) (Map.assocs mOut) visits status)
    to1 g (ValueExternThunk _ lvl nm params bindings _ outcome)
      = let title = case lvl of
                      Level_Abstract nm' -> nm'
                      _                  -> nm
            context = ""
            inps = inputs params
            (mIn,mOut) = Map.partitionWithKey (\k _ -> k `elem` inps) bindings
        in do (visits,status) <- getOutcome outcome
              return (DTNode g title context (Map.assocs mIn) (Map.assocs mOut) visits status)
    to1 g v
      = return (DTLeaf g v)

    getOutcome outcome
      = do vG <- expand outcome
           case vG of
             ValueGuess _ -> return ([], Failure ("no outcome record for: " ++ show vG))
             _            -> do v <- resolve vG
                                case v of
                                  Visit nm subderivs messages next -> do (xs,status) <- getOutcome next
                                                                         subIds      <- mapM (const nextUnique) subderivs
                                                                         subtrees    <- mapM (\(g,(k,v)) -> to g v >>= \d -> return (k,g,d)) (zip subIds subderivs)
                                                                         return (DTVisit nm subtrees (Foldable.toList messages) : xs, status)
                                  Finish status                    -> return ([], status)

resultHandler :: Opts -> DerivationTree -> IT IO ()
resultHandler opts root = do tree <- if expansion opts == ExpandFull
                                     then expandTree root
                                     else return root
                             str <- finishTTM (toStringM tree)
                             writeResult str
  where
    toStringM :: DerivationTree -> TTM IO String
    toStringM tree
      | targetFormat opts == Dot  = treeToDotStringTT tree
      | targetFormat opts == Text = treeToStringTT tree
      | targetFormat opts == Tex  = treeToTexStringTT False tree
      | targetFormat opts == None = return "<option -Tnone is set, derivation is not printed>"

    writeResult str
      | targetFormat opts == None = return () -- no derivation output
      | otherwise = do handle <- case outputFile opts of
                                  Just nm -> liftIO $ openFile nm WriteMode
                                  Nothing -> return stdout
                       liftIO $ do hPutStrLn handle str
                                   hFlush handle
                                   case outputFile opts of
                                     Just _ -> hClose handle
                                     _      -> return ()

main :: IO ()
main = do opts <- commandlineArgs
          -- let opts = defaultOpts { sourceFile = "examples/helloworld.rul" }
          mbExpr  <- tryInclude ExprParser (sourceFile opts)
          mbStmts <- mapM (tryInclude StmtsParser) (extraFiles opts)
          case mbExpr of
            Nothing   -> return ()
            Just expr -> let stmts = concat $ catMaybes mbStmts
                             (mbAct, mbErr) = evaluate (resultHandler opts) opts stmts expr
                         in do maybe (return ()) (hPutStrLn stderr) mbErr
                               maybe (return ()) id mbAct
     where
       tryInclude :: Select a -> FilePath -> IO (Maybe a)
       tryInclude sel path
         = do txt <- readFile path
              let tks = tokenize path txt
                  res = parse sel tks
              case res of
                Left msgs -> do hPutStrLn stderr (head msgs)
                                return Nothing
                Right v   -> return (Just v)

