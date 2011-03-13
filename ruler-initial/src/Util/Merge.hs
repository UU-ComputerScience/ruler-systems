{-# OPTIONS -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances -fallow-undecidable-instances #-}
module Util.Merge where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set


--
-- Combines maps, merging common keys, with a list at the leaves.
--

class Mergeable a where
  merge :: a -> a -> a

instance Mergeable [a] where
  merge = (++)

instance Ord a => Mergeable (Set a) where
  merge = Set.union

instance (Mergeable a, Ord k) => Mergeable (Map k a) where
  merge = Map.unionWith merge


--
-- In a sequence of maps with a list at the leaves, gives a map with
-- the first element of that list, and the remainder.
--

class Nub a b | a -> b where
  dups    :: a -> (a, b)

instance Nub [a] a where
  dups (x:xs) = (xs, x)

instance (Empty a, Nub a b, Ord k) => Nub (Map k a) (Map k b) where
  dups mp = let (pairsRest, pairsSingle) = Map.foldWithKey step ([], []) mp
                step k v (rs,ss)
                  | isEmpty v = (rs, ss)
                  | otherwise = let (r,s) = dups v in ((k,r) : rs, (k,s) : ss)
            in (Map.fromAscList pairsRest, Map.fromAscList pairsSingle)


--
-- When does a container not have any values
--

class Empty a where
  isEmpty :: a -> Bool

instance Empty [a] where
  isEmpty = null

instance Empty a => Empty (Map k a) where
  isEmpty = all isEmpty . Map.elems

