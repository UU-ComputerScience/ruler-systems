--
-- Left-Corner transform of a grammar
-- (this implementation assumes that the empty sequence cannot be derived)
--

module Util.Grammar.LeftCorner(leftcorner) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set

import Util.Common
import Util.Grammar.Grammar

import Debug.Trace


--
-- Determine the set of left recursive identifiers
--

leftrecursive :: Map NtIdent (Set NtIdent) -> Set NtIdent
leftrecursive mp
  = Set.fromList [ nt | (nt, cs) <- Map.assocs mp, nt `Set.member` cs ]


leftcorners :: Grammar -> Map NtIdent (Set NtIdent)
leftcorners g
  = if null fixp
    then Map.empty
    else snd $ head fixp
  where
    fixp = dropWhile (uncurry (/=)) $ zip out (tail out)

    out = Map.foldWithKey step (Map.empty : out) g

    step nt prods inp
      = zipWith (Map.insert nt) (combineWithUnion $ map (firstSym inp) prods) inp

    combineWithUnion
      = foldr (zipWith Set.union) infEmptySets

    infEmptySets
      = repeat Set.empty

    firstSym inp (Production_Prod _ ((Symbol_Nonterm ref):_))
      = map (Set.insert ref . Map.findWithDefault Set.empty ref) inp
    firstSym _ _
      = infEmptySets


--
-- Apply leftcorner transform
--

leftcorner :: Grammar -> Grammar
leftcorner gIn
  = gOut
  where
    corners = leftcorners gIn
    leftrecs = leftrecursive corners
    (gleft, gright) = Map.partitionWithKey (\nt _ -> nt `Set.member` leftrecs) gIn
    gOut = Map.foldWithKey (\nt prods g -> processNt gIn nt $ foldr (\(Production_Prod sem (s : ss)) (t,lcs) -> (add (ProdSem_Lam sem) (addSuf nt s) ss t, s : lcs)) (g, []) prods) gright gleft
    
processNt :: Grammar -> NtIdent -> (Grammar, [Symbol]) -> Grammar
processNt g nt
  = rec Set.empty
  where
    rec _ (t, [])
      = t
    rec done (t, lc:lcs)
      | lc `Set.member` done
          = rec done (t, lcs)
      | otherwise
          = case lc of
              Symbol_Nonterm ref -> let prods = Map.findWithDefault [] ref g
                                        (t', lcs') = foldr (\(Production_Prod sem (s : ss)) (t,newLcs) -> (add (ProdSem_Lam (ProdSem_App sem)) (addSuf nt s) (ss ++ [Symbol_Nonterm $ addSuf nt lc]) t, s : newLcs))  (t, []) prods
                                    in rec done' (t', lcs' ++ lcs)
              _                  -> rec done' (add (ProdSem_App ProdSem_Identity) nt [lc, Symbol_Nonterm $ addSuf nt lc] t, lcs)
      where done' = Set.insert lc done

add :: ProdSem -> NtIdent -> Symbols -> Grammar -> Grammar
add sem nt ss
  = Map.insertWith (++) nt [Production_Prod sem ss]

