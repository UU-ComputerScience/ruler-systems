module Env(emptyEnv, enter, leave, dups, missing, extend, find, Env, split, merge) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(nub)

newtype Env k v = Env [Map k (Entries k v)]

data Entries k v
  =  E [k] [(k,v)]

emptyEnv :: Env k v
emptyEnv = Env [Map.empty]

enter :: Env k v -> Env k v
enter (Env mps) = Env (Map.empty : mps)

leave :: Env k v -> (Env k v, Env k v)
leave (Env (mp : mps)) = (Env mps, Env [mp])

split :: Env k v -> (Env k v, Env k v)
split x = (x, x)

merge :: Ord k => Env k v -> Env k v -> Env k v
merge (Env (ha:ta)) (Env (hb:_))
  = Env (Map.unionWith mergeE ha hb : ta)

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
