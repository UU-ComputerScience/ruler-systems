module DepAnalysis {- (analyze) -} where

import Common
import Data.Graph
import Data.Tree
import Data.Maybe
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import Data.List(partition, nub)

analyze :: [DepItem] -> [DepItem] -> Set DepItem -> [(Reason, DepItem, [DepItem])] -> [SCC DepItem]
analyze oStartVisits oEndVisits avoidVisits deps = map compToItem comps
  where
    comps = stronglyConnCompR deps'

    compToItem (AcyclicSCC d) = AcyclicSCC (toItem d)
    compToItem (CyclicSCC ds) = CyclicSCC (map toItem ds)
    toItem (_,d,_) = d

    (gIn, vertexToKey, keyToVertex) = graphFromEdges (merge deps)
    gInRev = transposeG gIn
    deps' =  merge (map (\(d,(b,_)) -> (ReasonAlloc, d, [b])) extraConstrs ++ deps)
    
    merge ds = map (\(d,(r,ds)) -> (r,d,ds)) $ Map.assocs $
                 Map.fromListWith (\(_,as) (r,bs) -> (r,nub (as ++ bs))) $ map (\(r,d,ds) -> (d, (r,ds))) ds

    sequenceMap = let pairs = zip oStartVisits oEndVisits
                      seqs []             = []
                      seqs l@((x,_) : zs) = (x, l) : seqs zs
                   in Map.fromList (seqs pairs)  -- given an beginVisit: gives the end visits starting at this visit
    visitAllocMap = Map.mapWithKey allocStmt $ Map.filterWithKey (\k _ -> isStmt k) pairRanges
    extraConstrs  = [ (k,p) | (k,Just p) <- Map.assocs visitAllocMap ]

    allocStmt :: DepItem -> (DepItem, DepItem) -> Maybe (DepItem, DepItem)
    allocStmt dep (begin,end)
      | begin == end = Nothing
      | otherwise = if null ok
                    then if null bad
                         then Nothing
                         else Just (last bad)
                    else Just (last ok)
                  where
                    s = Map.findWithDefault [] begin sequenceMap
                    s' = untilVisit end s
                    (bad, ok) = partition (\(b,e) -> e `Set.member` avoidVisits) s'

    untilVisit x [] = []
    untilVisit x (p@(_,e):ys)
      | x == e    = [p]
      | otherwise = p : untilVisit x ys

    isStmt (DepMatch   _) = True
    isStmt (DepAssert  _) = True
    isStmt (DepDefault _) = True
    isStmt (DepAttach  _) = True
    isStmt (DepInvoke  _) = True
    isStmt _              = False

    endRanges   = markNodes gIn oEndVisits                  -- lastest visits
    beginRanges = markNodes gInRev (reverse oStartVisits)   -- earliest visits
    pairRanges  = Map.mapWithKey (\k v -> (Map.findWithDefault k k beginRanges, v)) endRanges
    rootItem    = DepVisStart [ident "_root"]

    markNodes :: Graph -> [DepItem] -> Map DepItem DepItem
    markNodes g sources = snd $ execState (travForest forest) (Nothing, Map.empty)
      where
        sources' = Set.fromList sources
        forest = dfs g (catMaybes $ map keyToVertex sources)
        travForest trees = mapM_ travTree trees
        travTree (Node v sub)
          = do let (_, d, _) = vertexToKey v
               encounter d
               travForest sub
        
        encounter d
          = do (mbD, mp) <- get
               if d `Set.member` sources'
                then put (Just d, mp)
                else case mbD of
                       Nothing -> return ()
                       Just d' -> put (mbD, Map.insertWith (flip const) d d' mp)
