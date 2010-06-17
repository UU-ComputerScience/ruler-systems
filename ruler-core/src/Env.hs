module Env( emptyEnv, enterWith, splitWith, enter, leave, dups, missing
          , extend, extendTail, find, Env, split, merge, intersection, strip, push, assocs) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(nub)

newtype Env k v = Env [Map k (Entries k v)]

data Entries k v
  = E [k] [(k,v)]

instance (Show k, Show v) => Show (Env k v) where
  show (Env mps) = show (map Map.assocs mps)

instance (Show k, Show v) => Show (Entries k v) where
  show (E ms ks) = "<E: " ++ show ms ++ " * " ++ show ks ++ ">"

emptyEnv :: Env k v
emptyEnv = Env [Map.empty]

enter :: Env k v -> Env k v
enter (Env mps) = Env (Map.empty : mps)

enterWith :: Env k v -> Env k v -> Env k v
enterWith (Env (mp : _)) (Env mps) = Env (mp : mps)

leave :: Env k v -> (Env k v, Env k v)
leave (Env (mp : mps)) = (Env mps, Env [mp])

split :: Env k v -> (Env k v, Env k v)
split x = (enter x, x)

splitWith :: Env k v -> Env k v -> (Env k v, Env k v)
splitWith l x = (enterWith l x, x)

-- result: merged env, left only env
merge :: Ord k => Env k v -> Env k v -> (Env k v, Env k v)
merge (Env (ha:t)) (Env t1)
  | length t /= length t1 = error "input environments from different scopes"
  | otherwise             = (Env t2, Env [l])
  where
    tstar  = Map.unions t1
    (l,t2) = apply ha tstar t1

apply :: Ord k => Map k (Entries k v) -> Map k (Entries k v) -> [Map k (Entries k v)] -> (Map k (Entries k v), [Map k (Entries k v)])   
apply mp mpstar mps
  = (new `Map.union` mpDefs, mps')
  where
    (mpElse, mpDefs) = Map.partition (\(E _ ds) -> null ds) mp
    (old,new) = Map.partitionWithKey (\k _ -> Map.member k mpstar) mpElse
    mps' = Map.foldWithKey mergeEnt mps old

mergeEnt :: Ord k => k -> Entries k v -> [Map k (Entries k v)] -> [Map k (Entries k v)]
mergeEnt k e (mp:mps)
  | k `Map.member` mp = Map.insertWith mergeE k e mp : mps
  | otherwise         = mp : mergeEnt k e mps

mergeE :: Entries k v -> Entries k v -> Entries k v
mergeE (E ms1 ds1) (E ms2 ds2) = E (ms1 ++ ms2) (ds1 ++ ds2)

dups :: Ord k => Env k v -> [[(k,v)]]
dups (Env mps) = foldl (Map.fold dups') [] mps
  where dups' (E _ defs) | length defs > 1 = (defs :)
                         | otherwise       = id

missing :: Ord k => Env k v -> [[k]]
missing (Env mps) = foldl (Map.fold miss') [] mps
  where miss' (E ks _) | null ks   = id
                       | otherwise = (ks :)

extend :: Ord k => k -> v -> Env k v -> Env k v
extend k v (Env (mp:mps))
  = case Map.lookup k mp of
      Nothing        -> Env (Map.insert k (E [] [(k,v)]) mp : mps)
      Just (E ms ps) -> Env (Map.insert k (E ms ((k,v) : ps)) mp : mps)

extendTail :: Ord k => k -> v -> Env k v -> Env k v
extendTail k v (Env (mp:mps))
  = case Map.lookup k mp of
      Nothing        -> Env (Map.insert k (E [] [(k,v)]) mp : mps)
      Just (E ms ps) -> Env (Map.insert k (E ms (ps ++ [(k,v)])) mp : mps)

find :: Ord k => k -> v -> Env k v -> (v, Env k v)
find k defl e@(Env mps)
  = case find' mps of
      Just (v, mps') -> (v, Env mps')
      Nothing        -> (defl, Env (Map.insertWith mergeE k (E [k] []) (head mps) : tail mps ))
  where
    find' [] = Nothing
    find' ms@(mp : mps)
      = case Map.lookup k mp of
          Just (E _ ((_,v):_)) -> return (v, ms)
          _ -> do (v,mps') <- find' mps
                  return (v, mp : mps')

intersection :: Ord k => [Env k v] -> Env k v
intersection []    = emptyEnv
intersection [env] = env
intersection (x:xs) = inter x (intersection xs)
  where
    inter (Env (a:l)) (Env (b:r)) = Env (inter' a b : zipWith inter' l r)
    inter' = Map.intersectionWith mergeE

strip :: Env k v -> Env k v
strip (Env t) = Env (map (Map.map strip') t)
  where strip' (E _ ds) = E [] (trim ds)
        trim [] = []
        trim (x:_) = [x]

push :: Ord k => Env k v -> Env k v -> Env k v
push (Env (m : _)) (Env (m' : t))
  = Env (Map.unionWith mergeE m m' : t)

assocs :: Ord k => Env k v -> [(k,v)]
assocs (Env ms)
  = [ head ds | (k, E _ ds) <- acs, not (null ds) ]
  where acs = Map.assocs $ Map.unions ms
