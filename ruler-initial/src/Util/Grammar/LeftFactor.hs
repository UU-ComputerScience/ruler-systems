--
-- Left Factor transform of a grammar
--  (Primitive form of refactoring: doesn't look through nonterminals yet)
--

module Util.Grammar.LeftFactor(leftfactor) where

import Data.List
import Data.Maybe
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set

import Util.Common
import Util.Grammar.Grammar


leftfactor :: Grammar -> Grammar
leftfactor gIn
  = Map.foldWithKey (\nt prods g -> processNt nt g) gIn gIn

processNt :: NtIdent -> Grammar -> Grammar
processNt nt
  = rec
  where
    rec gIn
      = if null attempts
        then gIn
        else rec gNext
      where
        prods = Map.findWithDefault [] nt gIn

        attempts = catMaybes (map commonFactor prods)

        commonFactor (Production_Prod _ (s:_))
          | length matches > 1 = Just (s, matches, rest)
          where
            (matches, rest) = partition isMatch prods
            isMatch (Production_Prod _ (c:_)) = c == s
            isMatch _                         = False
        commonFactor _
          = Nothing

        ((s, ps, qs):_) = attempts
        nt'   = addSuf nt s
        p'    = Production_Prod (ProdSem_App ProdSem_Identity) [ s, Symbol_Nonterm nt' ]
        ps'   = map (\(Production_Prod sem (_:cs)) -> Production_Prod (ProdSem_Lam sem) cs) ps
        gNext = Map.insert nt' ps' $ Map.insert nt (p' : qs) gIn

