

-- UUAGC 0.9.8 (Experiment.ag)
module Main where

{-# LINE 10 "Experiment.ag" #-}

import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Maybe
import qualified Data.IntMap as IntMap
import Data.IntMap(IntMap)
import Data.List
{-# LINE 17 "Experiment.hs" #-}
{-# LINE 94 "Experiment.ag" #-}

type Idents = [Ident]
type Ident = Delay
{-# LINE 22 "Experiment.hs" #-}

{-# LINE 105 "Experiment.ag" #-}

mkExprDeriv :: Subst -> Expr -> (ExprDeriv, Subst)
mkExprDeriv subst expr
  = let inh = Inh_ExprRoot { s_Inh_ExprRoot = subst }
        sem = sem_ExprRoot (ExprRoot_Root expr)
        syn = wrap_ExprRoot sem inh
     in (deriv_Syn_ExprRoot syn, s_Syn_ExprRoot syn)
{-# LINE 32 "Experiment.hs" #-}

{-# LINE 166 "Experiment.ag" #-}

data Subst = Subst !Int (IntMap Value)
  deriving Show

data Value
  = ValType Ty
  | ValDelay Delay
  | ValDelayed DelayedSem

instance Show Value where
  show (ValType ty)   = show ty
  show (ValDelay d)   = show d
  show (ValDelayed _) = show "<<delayed>>" 

emptySubst :: Subst
emptySubst = Subst 0 IntMap.empty

singleSubst :: Delay -> Value -> Subst
singleSubst (Delay i) v = Subst 0 (IntMap.singleton i v)

class ApplySubst a where
  appSubst :: Subst -> a -> a

instance ApplySubst Subst where
  appSubst s1@(Subst i1 m1) (Subst i2 m2)
    = Subst (i1 `max` i2) (IntMap.union m1 m2')
    where m2' = IntMap.map (appSubst s1) m2

instance ApplySubst Value where
  appSubst s (ValType ty) = ValType $ appSubst s ty
  appSubst s@(Subst _ m) v@(ValDelay (Delay i))
    = case IntMap.lookup i m of
        Just (ValDelay d') -> ValDelay d'
        Just (ValType ty)  -> ValType ty
        _                  -> v
  appSubst _ d = d

instance ApplySubst Delay where
  appSubst s@(Subst _ m) d@(Delay i)
    = case IntMap.lookup i m of
        Just (ValDelay d') -> d'
        _                  -> d

instance ApplySubst Ty where
  appSubst s@(Subst _ m) ty
    = case ty of
        Ty_Con nm args   -> Ty_Con nm (map (appSubst s) args)
        Ty_Univ ids body -> Ty_Univ (map (appSubst s) ids) (appSubst s body)
        Ty_Var (Delay i) -> case IntMap.lookup i m of
                              Just (ValType ty') -> ty'
                              Just (ValDelay d)  -> Ty_Var d
                              _                  -> ty
{-# LINE 87 "Experiment.hs" #-}

{-# LINE 225 "Experiment.ag" #-}

data Delay = Delay Int
  deriving (Show, Eq, Ord)

type DelayedSem = Subst -> Ty -> (Ty, Subst)
type ConditionalDelayedSem = Subst -> Ty -> Maybe (Ty, Subst)

fresh :: Subst -> (Delay, Subst)
fresh = delay (\s ty -> (ty, s))

delay :: DelayedSem -> Subst -> (Delay, Subst)
delay f (Subst n m)
  = (Delay n', Subst n' m')
  where
    n' = n + 1
    m' = IntMap.insert n' (ValDelayed f) m

force :: Delay -> Ty -> Subst -> (Ty, Subst)
force d@(Delay i) ty s@(Subst _ m)
  = case IntMap.lookup i m of
      Just (ValDelayed f) -> let (ty', s') = f s (s `appSubst` ty)
                                 ty'' = s' `appSubst` ty'
                             in (ty'', singleSubst d (ValType ty'') `appSubst` s')
      Just (ValType ty')  -> (ty', s)
      _ -> error "force: delay-indirection not in substitution"

semantics :: Delay -> Subst -> DelayedSem
semantics (Delay i) (Subst _ m)
  = case IntMap.findWithDefault (error "semantics: delay not in subst") i m of
      ValDelayed f -> f
      _            -> error "semantics: substitution target mismatch"

compose :: Delay -> Delay -> Subst -> Subst
compose d1 d2 s
  = (singleSubst d1 v `appSubst` singleSubst d2 v) `appSubst` s'
  where
    f1 = semantics d1 s
    f2 = semantics d2 s
    f s t = let (t', s') = f2 s t
             in f1 s' t' 
    (d, s') = delay f s
    v = ValDelay d

fixate :: Ty -> Subst -> (Ty, Subst)
fixate t s
  = let s' = foldr f s (delays t)
        f d s = case s `appSubst` (Ty_Var d) of
                  Ty_Var d' -> let (d'',s') = fresh s
                               in snd $ force d' (Ty_Var d'') s'
                  _         -> s
    in (s' `appSubst` t, s')


multi :: [ConditionalDelayedSem] -> DelayedSem
multi sems s t
  = case catMaybes $ map (\f -> f s t) sems of
      []    -> error "no matching conditional semantics"
      (x:_) -> x

delays :: Ty -> [Delay]
delays t
  = case t of
      Ty_Con _ args -> concatMap delays args
      Ty_Univ idents body -> nub (delays body) \\ idents 
      Ty_Var ident        -> [ident]
{-# LINE 155 "Experiment.hs" #-}

{-# LINE 297 "Experiment.ag" #-}

unify :: Subst -> Ty -> Ty -> Subst
unify s (Ty_Var ident1) (Ty_Var ident2) = compose ident1 ident2 s
unify s (Ty_Var ident) ty = discover s ident ty
unify s ty (Ty_Var ident) = discover s ident ty
unify s (Ty_Con nm1 tps1) (Ty_Con nm2 tps2)
  | nm1 /= nm2 = error "unify: type constructor clash"
  | length tps1 /= length tps2 = error "unify: kind error"
  | otherwise = foldr (\(t1,t2) s' -> unify s' (s' `appSubst` t1) (s' `appSubst` t2)) s $ zip tps1 tps2
unify s (Ty_Univ vars1 tp1) (Ty_Univ vars2 tp2)
  | length vars1 == length vars2 =
      let s' = foldr ($) s (zipWith compose vars1 vars2)
       in unify s' (s `appSubst` tp1) (s `appSubst` tp2)
unify s _ _ = error "unify: one of the types is not general enough"

discover :: Subst -> Delay -> Ty -> Subst
discover s d ty
  | d `elem` delays ty = error "discover: occur check"
  | otherwise          = snd $ force d ty s
{-# LINE 177 "Experiment.hs" #-}

{-# LINE 387 "Experiment.ag" #-}

mkArr :: Ty -> Ty -> Ty
mkArr t1 t2 = Ty_Con "->" [t1, t2]

instantiate :: Ty -> Subst -> (Ty, Subst)
instantiate t@(Ty_Univ idents ty) s
  = (rename ty, s')
  where
    (m, s') = foldr (\d (m,s) -> let (d',s') = fresh s in (Map.insert d d' m, s')) (Map.empty, s) idents

    rename (Ty_Con nm args)   = Ty_Con nm (map rename args)
    rename (Ty_Univ ids body) = Ty_Univ ids (rename body)
    rename (Ty_Var ident)     = case Map.lookup ident m of
                                  Just d  -> Ty_Var d
                                  Nothing -> Ty_Var ident
{-# LINE 195 "Experiment.hs" #-}

{-# LINE 407 "Experiment.ag" #-}

inferDeriv :: Subst -> ExprDeriv -> (Ty, Subst)
inferDeriv s deriv
  = (ty_Syn_ExprDeriv syn, s_Syn_ExprDeriv syn)
  where
    inh = Inh_ExprDeriv { s_Inh_ExprDeriv = s, g_Inh_ExprDeriv = Map.empty }
    sem = sem_ExprDeriv deriv
    syn = wrap_ExprDeriv sem inh

infer :: Expr -> Ty
infer e = tp
  where
    (tp, _) = inferDeriv s deriv
    (deriv, s) = mkExprDeriv emptySubst e

main :: IO ()
main = let e1 = Expr_Lam2 "c" (Type_Con "Int" []) e2
           e2 = Expr_App e3 e4
           e3 = Expr_Lam2 "f" (Type_Univ ["a"] (Type_Con "->" [Type_Var "a", Type_Var "a"])) e31
           e31 = Expr_App (Expr_Var "f") (Expr_Var "c")
           e4 = Expr_Lam1 "x" (Expr_Var "x")
           ty = infer e1
        in putStrLn (show ty)
{-# LINE 221 "Experiment.hs" #-}
-- Expr --------------------------------------------------------
data Expr  = Expr_App (Expr) (Expr) 
           | Expr_Lam1 (String) (Expr) 
           | Expr_Lam2 (String) (Type) (Expr) 
           | Expr_Var (String) 
           deriving ( Show)
-- cata
sem_Expr :: Expr  ->
            T_Expr 
sem_Expr (Expr_App _f _a )  =
    (sem_Expr_App (sem_Expr _f ) (sem_Expr _a ) )
sem_Expr (Expr_Lam1 _x _e )  =
    (sem_Expr_Lam1 _x (sem_Expr _e ) )
sem_Expr (Expr_Lam2 _x _tp _e )  =
    (sem_Expr_Lam2 _x (sem_Type _tp ) (sem_Expr _e ) )
sem_Expr (Expr_Var _x )  =
    (sem_Expr_Var _x )
-- semantic domain
type T_Expr  = Subst ->
               ( ExprDeriv,Subst)
sem_Expr_App :: T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_App f_ a_  =
    (\ _lhsIs ->
         (let _lhsOderiv :: ExprDeriv
              _lhsOs :: Subst
              _fOs :: Subst
              _aOs :: Subst
              _fIderiv :: ExprDeriv
              _fIs :: Subst
              _aIderiv :: ExprDeriv
              _aIs :: Subst
              _lhsOderiv =
                  {-# LINE 127 "Experiment.ag" #-}
                  ExprDeriv_App _fIderiv (ExprDeriv_Gen _aIderiv)
                  {-# LINE 258 "Experiment.hs" #-}
              _lhsOs =
                  {-# LINE 116 "Experiment.ag" #-}
                  _aIs
                  {-# LINE 262 "Experiment.hs" #-}
              _fOs =
                  {-# LINE 115 "Experiment.ag" #-}
                  _lhsIs
                  {-# LINE 266 "Experiment.hs" #-}
              _aOs =
                  {-# LINE 115 "Experiment.ag" #-}
                  _fIs
                  {-# LINE 270 "Experiment.hs" #-}
              ( _fIderiv,_fIs) =
                  (f_ _fOs )
              ( _aIderiv,_aIs) =
                  (a_ _aOs )
          in  ( _lhsOderiv,_lhsOs)))
sem_Expr_Lam1 :: String ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_Lam1 x_ e_  =
    (\ _lhsIs ->
         (let _lhsOderiv :: ExprDeriv
              _lhsOs :: Subst
              _eOs :: Subst
              _eIderiv :: ExprDeriv
              _eIs :: Subst
              _lhsOderiv =
                  {-# LINE 129 "Experiment.ag" #-}
                  ExprDeriv_Lam1 x_ _eIderiv
                  {-# LINE 289 "Experiment.hs" #-}
              _lhsOs =
                  {-# LINE 116 "Experiment.ag" #-}
                  _eIs
                  {-# LINE 293 "Experiment.hs" #-}
              _eOs =
                  {-# LINE 115 "Experiment.ag" #-}
                  _lhsIs
                  {-# LINE 297 "Experiment.hs" #-}
              ( _eIderiv,_eIs) =
                  (e_ _eOs )
          in  ( _lhsOderiv,_lhsOs)))
sem_Expr_Lam2 :: String ->
                 T_Type  ->
                 T_Expr  ->
                 T_Expr 
sem_Expr_Lam2 x_ tp_ e_  =
    (\ _lhsIs ->
         (let _tpOm :: (Map String Ident)
              _lhsOderiv :: ExprDeriv
              _lhsOs :: Subst
              _tpOs :: Subst
              _eOs :: Subst
              _tpIm :: (Map String Ident)
              _tpIs :: Subst
              _tpIty :: Ty
              _eIderiv :: ExprDeriv
              _eIs :: Subst
              _tpOm =
                  {-# LINE 131 "Experiment.ag" #-}
                  Map.empty
                  {-# LINE 320 "Experiment.hs" #-}
              _lhsOderiv =
                  {-# LINE 132 "Experiment.ag" #-}
                  ExprDeriv_Lam2 x_ _tpIty _eIderiv
                  {-# LINE 324 "Experiment.hs" #-}
              _lhsOs =
                  {-# LINE 116 "Experiment.ag" #-}
                  _eIs
                  {-# LINE 328 "Experiment.hs" #-}
              _tpOs =
                  {-# LINE 137 "Experiment.ag" #-}
                  _lhsIs
                  {-# LINE 332 "Experiment.hs" #-}
              _eOs =
                  {-# LINE 115 "Experiment.ag" #-}
                  _tpIs
                  {-# LINE 336 "Experiment.hs" #-}
              ( _tpIm,_tpIs,_tpIty) =
                  (tp_ _tpOm _tpOs )
              ( _eIderiv,_eIs) =
                  (e_ _eOs )
          in  ( _lhsOderiv,_lhsOs)))
sem_Expr_Var :: String ->
                T_Expr 
sem_Expr_Var x_  =
    (\ _lhsIs ->
         (let _lhsOderiv :: ExprDeriv
              _lhsOs :: Subst
              _lhsOderiv =
                  {-# LINE 125 "Experiment.ag" #-}
                  ExprDeriv_Inst (ExprDeriv_Var x_)
                  {-# LINE 351 "Experiment.hs" #-}
              _lhsOs =
                  {-# LINE 116 "Experiment.ag" #-}
                  _lhsIs
                  {-# LINE 355 "Experiment.hs" #-}
          in  ( _lhsOderiv,_lhsOs)))
-- ExprDeriv ---------------------------------------------------
data ExprDeriv  = ExprDeriv_App (ExprDeriv) (ExprDeriv) 
                | ExprDeriv_Fixate (ExprDeriv) 
                | ExprDeriv_Gen (ExprDeriv) 
                | ExprDeriv_Inst (ExprDeriv) 
                | ExprDeriv_Lam1 (String) (ExprDeriv) 
                | ExprDeriv_Lam2 (String) (Ty) (ExprDeriv) 
                | ExprDeriv_Var (String) 
                deriving ( Show)
-- cata
sem_ExprDeriv :: ExprDeriv  ->
                 T_ExprDeriv 
sem_ExprDeriv (ExprDeriv_App _f _a )  =
    (sem_ExprDeriv_App (sem_ExprDeriv _f ) (sem_ExprDeriv _a ) )
sem_ExprDeriv (ExprDeriv_Fixate _e )  =
    (sem_ExprDeriv_Fixate (sem_ExprDeriv _e ) )
sem_ExprDeriv (ExprDeriv_Gen _e )  =
    (sem_ExprDeriv_Gen (sem_ExprDeriv _e ) )
sem_ExprDeriv (ExprDeriv_Inst _e )  =
    (sem_ExprDeriv_Inst (sem_ExprDeriv _e ) )
sem_ExprDeriv (ExprDeriv_Lam1 _x _e )  =
    (sem_ExprDeriv_Lam1 _x (sem_ExprDeriv _e ) )
sem_ExprDeriv (ExprDeriv_Lam2 _x _ty _e )  =
    (sem_ExprDeriv_Lam2 _x _ty (sem_ExprDeriv _e ) )
sem_ExprDeriv (ExprDeriv_Var _x )  =
    (sem_ExprDeriv_Var _x )
-- semantic domain
type T_ExprDeriv  = (Map String Ty) ->
                    Subst ->
                    ( Subst,Ty)
data Inh_ExprDeriv  = Inh_ExprDeriv {g_Inh_ExprDeriv :: Map String Ty,s_Inh_ExprDeriv :: Subst}
data Syn_ExprDeriv  = Syn_ExprDeriv {s_Syn_ExprDeriv :: Subst,ty_Syn_ExprDeriv :: Ty}
wrap_ExprDeriv :: T_ExprDeriv  ->
                  Inh_ExprDeriv  ->
                  Syn_ExprDeriv 
wrap_ExprDeriv sem (Inh_ExprDeriv _lhsIg _lhsIs )  =
    (let ( _lhsOs,_lhsOty) =
             (sem _lhsIg _lhsIs )
     in  (Syn_ExprDeriv _lhsOs _lhsOty ))
sem_ExprDeriv_App :: T_ExprDeriv  ->
                     T_ExprDeriv  ->
                     T_ExprDeriv 
sem_ExprDeriv_App f_ a_  =
    (\ _lhsIg
       _lhsIs ->
         (let _aOs :: Subst
              _lhsOs :: Subst
              _lhsOty :: Ty
              _fOg :: (Map String Ty)
              _fOs :: Subst
              _aOg :: (Map String Ty)
              _fIs :: Subst
              _fIty :: Ty
              _aIs :: Subst
              _aIty :: Ty
              _aOs =
                  {-# LINE 332 "Experiment.ag" #-}
                  _fIs
                  {-# LINE 415 "Experiment.hs" #-}
              __tup1 =
                  {-# LINE 333 "Experiment.ag" #-}
                  fresh _aIs
                  {-# LINE 419 "Experiment.hs" #-}
              (_d,_) =
                  {-# LINE 333 "Experiment.ag" #-}
                  __tup1
                  {-# LINE 423 "Experiment.hs" #-}
              (_,_s1) =
                  {-# LINE 333 "Experiment.ag" #-}
                  __tup1
                  {-# LINE 427 "Experiment.hs" #-}
              _alpha =
                  {-# LINE 334 "Experiment.ag" #-}
                  Ty_Var _d
                  {-# LINE 431 "Experiment.hs" #-}
              _s' =
                  {-# LINE 336 "Experiment.ag" #-}
                  unify _s1     (_aIs `appSubst` _fIty) (mkArr _aIty _alpha    )
                  {-# LINE 435 "Experiment.hs" #-}
              _lhsOs =
                  {-# LINE 338 "Experiment.ag" #-}
                  _s'
                  {-# LINE 439 "Experiment.hs" #-}
              _lhsOty =
                  {-# LINE 339 "Experiment.ag" #-}
                  _s'     `appSubst` _alpha
                  {-# LINE 443 "Experiment.hs" #-}
              _fOg =
                  {-# LINE 325 "Experiment.ag" #-}
                  _lhsIg
                  {-# LINE 447 "Experiment.hs" #-}
              _fOs =
                  {-# LINE 323 "Experiment.ag" #-}
                  _lhsIs
                  {-# LINE 451 "Experiment.hs" #-}
              _aOg =
                  {-# LINE 325 "Experiment.ag" #-}
                  _lhsIg
                  {-# LINE 455 "Experiment.hs" #-}
              ( _fIs,_fIty) =
                  (f_ _fOg _fOs )
              ( _aIs,_aIty) =
                  (a_ _aOg _aOs )
          in  ( _lhsOs,_lhsOty)))
sem_ExprDeriv_Fixate :: T_ExprDeriv  ->
                        T_ExprDeriv 
sem_ExprDeriv_Fixate e_  =
    (\ _lhsIg
       _lhsIs ->
         (let _lhsOs :: Subst
              _lhsOty :: Ty
              _eOg :: (Map String Ty)
              _eOs :: Subst
              _eIs :: Subst
              _eIty :: Ty
              __tup2 =
                  {-# LINE 381 "Experiment.ag" #-}
                  fixate _eIty _eIs
                  {-# LINE 475 "Experiment.hs" #-}
              (_ty,_) =
                  {-# LINE 381 "Experiment.ag" #-}
                  __tup2
                  {-# LINE 479 "Experiment.hs" #-}
              (_,_lhsOs) =
                  {-# LINE 381 "Experiment.ag" #-}
                  __tup2
                  {-# LINE 483 "Experiment.hs" #-}
              _lhsOty =
                  {-# LINE 382 "Experiment.ag" #-}
                  let ds = nub $ delays _ty
                   in if null ds
                      then _ty
                      else Ty_Univ ds _ty
                  {-# LINE 490 "Experiment.hs" #-}
              _eOg =
                  {-# LINE 325 "Experiment.ag" #-}
                  _lhsIg
                  {-# LINE 494 "Experiment.hs" #-}
              _eOs =
                  {-# LINE 323 "Experiment.ag" #-}
                  _lhsIs
                  {-# LINE 498 "Experiment.hs" #-}
              ( _eIs,_eIty) =
                  (e_ _eOg _eOs )
          in  ( _lhsOs,_lhsOty)))
sem_ExprDeriv_Gen :: T_ExprDeriv  ->
                     T_ExprDeriv 
sem_ExprDeriv_Gen e_  =
    (\ _lhsIg
       _lhsIs ->
         (let __tup3 :: ((Ty,Subst))
              _lhsOty :: Ty
              _lhsOs :: Subst
              _eOg :: (Map String Ty)
              _eOs :: Subst
              _eIs :: Subst
              _eIty :: Ty
              __tup3 =
                  {-# LINE 366 "Experiment.ag" #-}
                  let (d, s') = delay (multi [fGen,fMono]) _eIs
                      fGen s t@(Ty_Univ ids t1)
                        = let s1 = unify s (s `appSubst` _eIty) t1
                              t2 = s1 `appSubst` t1
                              ids2 = map (appSubst s1) ids
                              t3 = Ty_Univ ids2 t2
                              s2 = unify s1 (s1 `appSubst` t) (t3)
                          in Just (s2 `appSubst` t3, s2)
                      fGen s _ = Nothing
                      fMono s t = case s `appSubst` _eIty of
                                    Ty_Var d -> Just $ force d t s
                                    ty'      -> let s' = unify s t ty'
                                                in Just (s' `appSubst` t, s')
                   in (Ty_Var d, s')
                  {-# LINE 530 "Experiment.hs" #-}
              (_lhsOty,_) =
                  {-# LINE 366 "Experiment.ag" #-}
                  __tup3
                  {-# LINE 534 "Experiment.hs" #-}
              (_,_lhsOs) =
                  {-# LINE 366 "Experiment.ag" #-}
                  __tup3
                  {-# LINE 538 "Experiment.hs" #-}
              _eOg =
                  {-# LINE 325 "Experiment.ag" #-}
                  _lhsIg
                  {-# LINE 542 "Experiment.hs" #-}
              _eOs =
                  {-# LINE 323 "Experiment.ag" #-}
                  _lhsIs
                  {-# LINE 546 "Experiment.hs" #-}
              ( _eIs,_eIty) =
                  (e_ _eOg _eOs )
          in  ( _lhsOs,_lhsOty)))
sem_ExprDeriv_Inst :: T_ExprDeriv  ->
                      T_ExprDeriv 
sem_ExprDeriv_Inst e_  =
    (\ _lhsIg
       _lhsIs ->
         (let __tup4 :: ((Ty,Subst))
              _lhsOty :: Ty
              _lhsOs :: Subst
              _eOg :: (Map String Ty)
              _eOs :: Subst
              _eIs :: Subst
              _eIty :: Ty
              __tup4 =
                  {-# LINE 349 "Experiment.ag" #-}
                  case _eIty of
                    Ty_Univ _ _ -> instantiate _eIty _eIs
                    _           -> (_eIty, _eIs)
                  {-# LINE 567 "Experiment.hs" #-}
              (_lhsOty,_) =
                  {-# LINE 349 "Experiment.ag" #-}
                  __tup4
                  {-# LINE 571 "Experiment.hs" #-}
              (_,_lhsOs) =
                  {-# LINE 349 "Experiment.ag" #-}
                  __tup4
                  {-# LINE 575 "Experiment.hs" #-}
              _eOg =
                  {-# LINE 325 "Experiment.ag" #-}
                  _lhsIg
                  {-# LINE 579 "Experiment.hs" #-}
              _eOs =
                  {-# LINE 323 "Experiment.ag" #-}
                  _lhsIs
                  {-# LINE 583 "Experiment.hs" #-}
              ( _eIs,_eIty) =
                  (e_ _eOg _eOs )
          in  ( _lhsOs,_lhsOty)))
sem_ExprDeriv_Lam1 :: String ->
                      T_ExprDeriv  ->
                      T_ExprDeriv 
sem_ExprDeriv_Lam1 x_ e_  =
    (\ _lhsIg
       _lhsIs ->
         (let _eOs :: Subst
              _eOg :: (Map String Ty)
              _lhsOty :: Ty
              _lhsOs :: Subst
              _eIs :: Subst
              _eIty :: Ty
              __tup5 =
                  {-# LINE 341 "Experiment.ag" #-}
                  fresh _lhsIs
                  {-# LINE 602 "Experiment.hs" #-}
              (_d,_) =
                  {-# LINE 341 "Experiment.ag" #-}
                  __tup5
                  {-# LINE 606 "Experiment.hs" #-}
              (_,_eOs) =
                  {-# LINE 341 "Experiment.ag" #-}
                  __tup5
                  {-# LINE 610 "Experiment.hs" #-}
              _alpha =
                  {-# LINE 342 "Experiment.ag" #-}
                  Ty_Var _d
                  {-# LINE 614 "Experiment.hs" #-}
              _eOg =
                  {-# LINE 343 "Experiment.ag" #-}
                  Map.insert x_ _alpha     _lhsIg
                  {-# LINE 618 "Experiment.hs" #-}
              _lhsOty =
                  {-# LINE 344 "Experiment.ag" #-}
                  mkArr (_eIs `appSubst` _alpha    ) _eIty
                  {-# LINE 622 "Experiment.hs" #-}
              _lhsOs =
                  {-# LINE 324 "Experiment.ag" #-}
                  _eIs
                  {-# LINE 626 "Experiment.hs" #-}
              ( _eIs,_eIty) =
                  (e_ _eOg _eOs )
          in  ( _lhsOs,_lhsOty)))
sem_ExprDeriv_Lam2 :: String ->
                      Ty ->
                      T_ExprDeriv  ->
                      T_ExprDeriv 
sem_ExprDeriv_Lam2 x_ ty_ e_  =
    (\ _lhsIg
       _lhsIs ->
         (let _eOg :: (Map String Ty)
              _lhsOty :: Ty
              _lhsOs :: Subst
              _eOs :: Subst
              _eIs :: Subst
              _eIty :: Ty
              _eOg =
                  {-# LINE 346 "Experiment.ag" #-}
                  Map.insert x_ ty_ _lhsIg
                  {-# LINE 646 "Experiment.hs" #-}
              _lhsOty =
                  {-# LINE 347 "Experiment.ag" #-}
                  mkArr (_eIs `appSubst` ty_) _eIty
                  {-# LINE 650 "Experiment.hs" #-}
              _lhsOs =
                  {-# LINE 324 "Experiment.ag" #-}
                  _eIs
                  {-# LINE 654 "Experiment.hs" #-}
              _eOs =
                  {-# LINE 323 "Experiment.ag" #-}
                  _lhsIs
                  {-# LINE 658 "Experiment.hs" #-}
              ( _eIs,_eIty) =
                  (e_ _eOg _eOs )
          in  ( _lhsOs,_lhsOty)))
sem_ExprDeriv_Var :: String ->
                     T_ExprDeriv 
sem_ExprDeriv_Var x_  =
    (\ _lhsIg
       _lhsIs ->
         (let _lhsOty :: Ty
              _lhsOs :: Subst
              _lhsOty =
                  {-# LINE 330 "Experiment.ag" #-}
                  Map.findWithDefault (error "var not in environment") x_ _lhsIg
                  {-# LINE 672 "Experiment.hs" #-}
              _lhsOs =
                  {-# LINE 324 "Experiment.ag" #-}
                  _lhsIs
                  {-# LINE 676 "Experiment.hs" #-}
          in  ( _lhsOs,_lhsOty)))
-- ExprRoot ----------------------------------------------------
data ExprRoot  = ExprRoot_Root (Expr) 
               deriving ( Show)
-- cata
sem_ExprRoot :: ExprRoot  ->
                T_ExprRoot 
sem_ExprRoot (ExprRoot_Root _body )  =
    (sem_ExprRoot_Root (sem_Expr _body ) )
-- semantic domain
type T_ExprRoot  = Subst ->
                   ( ExprDeriv,Subst)
data Inh_ExprRoot  = Inh_ExprRoot {s_Inh_ExprRoot :: Subst}
data Syn_ExprRoot  = Syn_ExprRoot {deriv_Syn_ExprRoot :: ExprDeriv,s_Syn_ExprRoot :: Subst}
wrap_ExprRoot :: T_ExprRoot  ->
                 Inh_ExprRoot  ->
                 Syn_ExprRoot 
wrap_ExprRoot sem (Inh_ExprRoot _lhsIs )  =
    (let ( _lhsOderiv,_lhsOs) =
             (sem _lhsIs )
     in  (Syn_ExprRoot _lhsOderiv _lhsOs ))
sem_ExprRoot_Root :: T_Expr  ->
                     T_ExprRoot 
sem_ExprRoot_Root body_  =
    (\ _lhsIs ->
         (let _lhsOderiv :: ExprDeriv
              _lhsOs :: Subst
              _bodyOs :: Subst
              _bodyIderiv :: ExprDeriv
              _bodyIs :: Subst
              _lhsOderiv =
                  {-# LINE 121 "Experiment.ag" #-}
                  ExprDeriv_Fixate _bodyIderiv
                  {-# LINE 710 "Experiment.hs" #-}
              _lhsOs =
                  {-# LINE 116 "Experiment.ag" #-}
                  _bodyIs
                  {-# LINE 714 "Experiment.hs" #-}
              _bodyOs =
                  {-# LINE 115 "Experiment.ag" #-}
                  _lhsIs
                  {-# LINE 718 "Experiment.hs" #-}
              ( _bodyIderiv,_bodyIs) =
                  (body_ _bodyOs )
          in  ( _lhsOderiv,_lhsOs)))
-- Ty ----------------------------------------------------------
data Ty  = Ty_Con (String) (Tys) 
         | Ty_Univ (Idents) (Ty) 
         | Ty_Var (Ident) 
         deriving ( Show)
-- cata
sem_Ty :: Ty  ->
          T_Ty 
sem_Ty (Ty_Con _nm _args )  =
    (sem_Ty_Con _nm (sem_Tys _args ) )
sem_Ty (Ty_Univ _idents _body )  =
    (sem_Ty_Univ _idents (sem_Ty _body ) )
sem_Ty (Ty_Var _ident )  =
    (sem_Ty_Var _ident )
-- semantic domain
type T_Ty  = ( )
sem_Ty_Con :: String ->
              T_Tys  ->
              T_Ty 
sem_Ty_Con nm_ args_  =
    (let 
     in  ( ))
sem_Ty_Univ :: Idents ->
               T_Ty  ->
               T_Ty 
sem_Ty_Univ idents_ body_  =
    (let 
     in  ( ))
sem_Ty_Var :: Ident ->
              T_Ty 
sem_Ty_Var ident_  =
    (let 
     in  ( ))
-- Type --------------------------------------------------------
data Type  = Type_Con (String) (Types) 
           | Type_Univ ([String]) (Type) 
           | Type_Var (String) 
           deriving ( Show)
-- cata
sem_Type :: Type  ->
            T_Type 
sem_Type (Type_Con _nm _tps )  =
    (sem_Type_Con _nm (sem_Types _tps ) )
sem_Type (Type_Univ _vars _body )  =
    (sem_Type_Univ _vars (sem_Type _body ) )
sem_Type (Type_Var _nm )  =
    (sem_Type_Var _nm )
-- semantic domain
type T_Type  = (Map String Ident) ->
               Subst ->
               ( (Map String Ident),Subst,Ty)
sem_Type_Con :: String ->
                T_Types  ->
                T_Type 
sem_Type_Con nm_ tps_  =
    (\ _lhsIm
       _lhsIs ->
         (let _lhsOty :: Ty
              _lhsOm :: (Map String Ident)
              _lhsOs :: Subst
              _tpsOm :: (Map String Ident)
              _tpsOs :: Subst
              _tpsIm :: (Map String Ident)
              _tpsIs :: Subst
              _tpsItys :: Tys
              _lhsOty =
                  {-# LINE 155 "Experiment.ag" #-}
                  Ty_Con nm_ _tpsItys
                  {-# LINE 790 "Experiment.hs" #-}
              _lhsOm =
                  {-# LINE 136 "Experiment.ag" #-}
                  _tpsIm
                  {-# LINE 794 "Experiment.hs" #-}
              _lhsOs =
                  {-# LINE 138 "Experiment.ag" #-}
                  _tpsIs
                  {-# LINE 798 "Experiment.hs" #-}
              _tpsOm =
                  {-# LINE 135 "Experiment.ag" #-}
                  _lhsIm
                  {-# LINE 802 "Experiment.hs" #-}
              _tpsOs =
                  {-# LINE 137 "Experiment.ag" #-}
                  _lhsIs
                  {-# LINE 806 "Experiment.hs" #-}
              ( _tpsIm,_tpsIs,_tpsItys) =
                  (tps_ _tpsOm _tpsOs )
          in  ( _lhsOm,_lhsOs,_lhsOty)))
sem_Type_Univ :: ([String]) ->
                 T_Type  ->
                 T_Type 
sem_Type_Univ vars_ body_  =
    (\ _lhsIm
       _lhsIs ->
         (let _lhsOty :: Ty
              _lhsOm :: (Map String Ident)
              _lhsOs :: Subst
              _bodyOm :: (Map String Ident)
              _bodyOs :: Subst
              _bodyIm :: (Map String Ident)
              _bodyIs :: Subst
              _bodyIty :: Ty
              _lhsOty =
                  {-# LINE 153 "Experiment.ag" #-}
                  Ty_Univ (catMaybes $ map (\x -> Map.lookup x _bodyIm) vars_) _bodyIty
                  {-# LINE 827 "Experiment.hs" #-}
              _lhsOm =
                  {-# LINE 136 "Experiment.ag" #-}
                  _bodyIm
                  {-# LINE 831 "Experiment.hs" #-}
              _lhsOs =
                  {-# LINE 138 "Experiment.ag" #-}
                  _bodyIs
                  {-# LINE 835 "Experiment.hs" #-}
              _bodyOm =
                  {-# LINE 135 "Experiment.ag" #-}
                  _lhsIm
                  {-# LINE 839 "Experiment.hs" #-}
              _bodyOs =
                  {-# LINE 137 "Experiment.ag" #-}
                  _lhsIs
                  {-# LINE 843 "Experiment.hs" #-}
              ( _bodyIm,_bodyIs,_bodyIty) =
                  (body_ _bodyOm _bodyOs )
          in  ( _lhsOm,_lhsOs,_lhsOty)))
sem_Type_Var :: String ->
                T_Type 
sem_Type_Var nm_  =
    (\ _lhsIm
       _lhsIs ->
         (let _lhsOs :: Subst
              _lhsOm :: (Map String Ident)
              _lhsOty :: Ty
              __tup6 =
                  {-# LINE 146 "Experiment.ag" #-}
                  case Map.lookup nm_ _lhsIm of
                    Just ident -> (ident, _lhsIs)
                    Nothing    -> fresh _lhsIs
                  {-# LINE 860 "Experiment.hs" #-}
              (_ident,_) =
                  {-# LINE 146 "Experiment.ag" #-}
                  __tup6
                  {-# LINE 864 "Experiment.hs" #-}
              (_,_lhsOs) =
                  {-# LINE 146 "Experiment.ag" #-}
                  __tup6
                  {-# LINE 868 "Experiment.hs" #-}
              _lhsOm =
                  {-# LINE 150 "Experiment.ag" #-}
                  Map.insert nm_ _ident     _lhsIm
                  {-# LINE 872 "Experiment.hs" #-}
              _lhsOty =
                  {-# LINE 151 "Experiment.ag" #-}
                  Ty_Var _ident
                  {-# LINE 876 "Experiment.hs" #-}
          in  ( _lhsOm,_lhsOs,_lhsOty)))
-- Types -------------------------------------------------------
type Types  = [(Type)]
-- cata
sem_Types :: Types  ->
             T_Types 
sem_Types list  =
    (Prelude.foldr sem_Types_Cons sem_Types_Nil (Prelude.map sem_Type list) )
-- semantic domain
type T_Types  = (Map String Ident) ->
                Subst ->
                ( (Map String Ident),Subst,Tys)
sem_Types_Cons :: T_Type  ->
                  T_Types  ->
                  T_Types 
sem_Types_Cons hd_ tl_  =
    (\ _lhsIm
       _lhsIs ->
         (let _lhsOtys :: Tys
              _lhsOm :: (Map String Ident)
              _lhsOs :: Subst
              _hdOm :: (Map String Ident)
              _hdOs :: Subst
              _tlOm :: (Map String Ident)
              _tlOs :: Subst
              _hdIm :: (Map String Ident)
              _hdIs :: Subst
              _hdIty :: Ty
              _tlIm :: (Map String Ident)
              _tlIs :: Subst
              _tlItys :: Tys
              _lhsOtys =
                  {-# LINE 158 "Experiment.ag" #-}
                  _hdIty : _tlItys
                  {-# LINE 911 "Experiment.hs" #-}
              _lhsOm =
                  {-# LINE 136 "Experiment.ag" #-}
                  _tlIm
                  {-# LINE 915 "Experiment.hs" #-}
              _lhsOs =
                  {-# LINE 138 "Experiment.ag" #-}
                  _tlIs
                  {-# LINE 919 "Experiment.hs" #-}
              _hdOm =
                  {-# LINE 135 "Experiment.ag" #-}
                  _lhsIm
                  {-# LINE 923 "Experiment.hs" #-}
              _hdOs =
                  {-# LINE 137 "Experiment.ag" #-}
                  _lhsIs
                  {-# LINE 927 "Experiment.hs" #-}
              _tlOm =
                  {-# LINE 135 "Experiment.ag" #-}
                  _hdIm
                  {-# LINE 931 "Experiment.hs" #-}
              _tlOs =
                  {-# LINE 137 "Experiment.ag" #-}
                  _hdIs
                  {-# LINE 935 "Experiment.hs" #-}
              ( _hdIm,_hdIs,_hdIty) =
                  (hd_ _hdOm _hdOs )
              ( _tlIm,_tlIs,_tlItys) =
                  (tl_ _tlOm _tlOs )
          in  ( _lhsOm,_lhsOs,_lhsOtys)))
sem_Types_Nil :: T_Types 
sem_Types_Nil  =
    (\ _lhsIm
       _lhsIs ->
         (let _lhsOtys :: Tys
              _lhsOm :: (Map String Ident)
              _lhsOs :: Subst
              _lhsOtys =
                  {-# LINE 159 "Experiment.ag" #-}
                  []
                  {-# LINE 951 "Experiment.hs" #-}
              _lhsOm =
                  {-# LINE 136 "Experiment.ag" #-}
                  _lhsIm
                  {-# LINE 955 "Experiment.hs" #-}
              _lhsOs =
                  {-# LINE 138 "Experiment.ag" #-}
                  _lhsIs
                  {-# LINE 959 "Experiment.hs" #-}
          in  ( _lhsOm,_lhsOs,_lhsOtys)))
-- Tys ---------------------------------------------------------
type Tys  = [(Ty)]
-- cata
sem_Tys :: Tys  ->
           T_Tys 
sem_Tys list  =
    (Prelude.foldr sem_Tys_Cons sem_Tys_Nil (Prelude.map sem_Ty list) )
-- semantic domain
type T_Tys  = ( )
sem_Tys_Cons :: T_Ty  ->
                T_Tys  ->
                T_Tys 
sem_Tys_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_Tys_Nil :: T_Tys 
sem_Tys_Nil  =
    (let 
     in  ( ))