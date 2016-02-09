{-# LANGUAGE FlexibleContexts #-}

module DepAnalysis (analyze) where

import Common
import Data.Graph
import Data.Tree
import Data.Maybe
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.IntMap as IntMap
import Data.IntMap(IntMap)
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.IntSet as IntSet
import Data.IntSet(IntSet)
import Data.List(partition, nub,sortBy,groupBy)
import Data.Array
import Data.Tree
import qualified Data.Graph.Inductive as G


-- computes additional dependencies related to "late-as-possible" scheduling and avoidance of
-- cyclic visits
analyze :: [DepItem] -> [DepItem] -> [DepItem] -> Set DepItem -> [(Reason, DepItem, [DepItem])] -> [SCC DepItem]
analyze oSemStarts oStartVisits oEndVisits avoidVisits deps = map compToItem comps
  where
    comps = stronglyConnCompR $ total $ deps'

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

-- Creates a total order
total :: [(Reason, DepItem, [DepItem])] -> [(Reason, DepItem, [DepItem])]
total deps = result
  where
    result = map (\(a,(r,bs)) -> (r,a,bs)) $ Map.assocs merged
    merged = Map.unionWith merge
               (Map.fromListWith merge [ (a, (ReasonOrder, [b])) | (b,a) <- pairs ])
               (Map.fromList [ (a,(r,bs)) | (r,a,bs) <- deps ])
    
    merge (_,as) (r,bs) = (r,nub (as ++ bs))
    toVal x = case G.lab g0 x of
                Nothing -> error ("has disapeared from graph: " ++ show x)
                Just v  -> v
    pairs = zip nodes3 (tail nodes3)
    nodes3 = map toVal nodes2
    
    items = nub ([ s | (_,s,_) <- deps ] ++ [ d | (_,_,ds) <- deps, d <- ds ])
    (nodes, mp) = G.mkNodes G.new items
    (Just edges) = G.mkEdges mp [ (s,d,r) | (r, s, ds) <- deps, d <- ds ]
    
    g0 :: G.Gr DepItem Reason
    g0 = G.mkGraph nodes edges     -- orig graph
    g1 = G.trc $ removeCycles g0   -- search graph
    
    nodes1 = sortBy compare' (G.nodes g0)  -- totally ordered
    compare' a b
      = let (Just av) = G.lab g0 a
            (Just bv) = G.lab g0 b
        in compare av bv
    
    dmap = list2DMap nodes1
    nodes2 = toListDMap $ fixSolve (assocsDMap dmap) dmap
    
    fixSolve todo dmap
      = case Map.maxViewWithKey todo of
          Nothing -> dmap
          Just ((a,v), todo1) ->
            let preds = G.pre g1 v  -- a must be before the preds
                troubles = filter (\b -> b < a) $ map (\v -> lookupDMapVal v dmap) preds
            in if null troubles
               then fixSolve todo1 dmap
               else let b = minimum troubles
                        succs = G.suc g1 v
                    in case insertDMap b v (removeDMap a dmap) of
                         Left dmap' ->  -- reindexed, rebuild todo
                           let a' = lookupDMapVal v dmap'
                               todo2 = Map.fromList [ (lookupDMapVal v dmap', v) | v <- Map.elems todo1 ]
                               todo3 = Map.fromList [ (k,v) | v <- succs, let k = lookupDMapVal v dmap', k > a'] `Map.union` todo2
                           in fixSolve todo3 dmap'
                         Right dmap' ->
                           let todo2 = Map.fromList [ (b,v) | v <- succs, let k = lookupDMapVal v dmap', b > a] `Map.union` todo1
                           in fixSolve todo2 dmap'

-- eliminates those edges E from the graph that make it cyclic.
-- for an edge e `elem` E, it holds that not removing it from
-- the graph:
-- (a) makes it cylic, or
-- (b) does not increase the connected-ness of nodes
-- the idea:
-- (1) compute strongly connected components
-- (2) arbitrarily totally-order the nodes in such component
-- (3) filter out those edges between nodes in such component
--     that are going in a different direction than imposed
--     by the order.
removeCycles :: G.Gr DepItem Reason -> G.Gr DepItem Reason
removeCycles g
  = G.efilter keep g
  where
    cmps = G.scc g
    mp = IntMap.unions (map createMp cmps)
    createMp comp =
      let items = IntSet.fromList comp
      in IntMap.fromList [ (i, items) | i <- comp ]
    
    keep (a, b, _)
      = case IntMap.lookup a mp of
          Nothing -> error "removeCycles: missing node"
          Just comp -> not (IntSet.member b comp) || b > a

-- A sparse array with elements indexed by doubles instead of integers,
-- offering fast insertion.

newtype DMap = DMap (Map Double Int, IntMap Double)

emptyDMap = DMap (Map.empty, IntMap.empty)

lookupDMapKey k (DMap (m,_)) = Map.findWithDefault undefined k m
lookupDMapVal v (DMap (_,m)) = IntMap.findWithDefault undefined v m

assocsDMap (DMap (m,_)) = Map.deleteMin $ Map.deleteMax m  -- remove sentinels

list2DMap xs = DMap (m, r)
  where
    pairs = zip [1.0, 2.0..] xs
    m = Map.fromAscList ([(0.0,0)] ++ pairs ++ [(fromIntegral (length xs + 1),maxBound)])
    r = IntMap.fromList [ (k,d) | (d,k) <- pairs ]

-- insert just before b (potentially reassigns the elements in the map)
insertDMap b v (DMap (m,r))
  | c == a    = let mp1' = Map.deleteMin mp1
                    mp2' = Map.deleteMax mp2
                in Left $ list2DMap (Map.elems mp1' ++ [v, v'] ++ Map.elems mp2')
  | otherwise = Right $ DMap (Map.insert c v m, IntMap.insert v c r)
  where
    (mp1,Just v',mp2) = Map.splitLookup b m
    (a,_) = Map.findMax mp1
    c = (a + b) / 2.0

removeDMap a (DMap (m,r))
  = DMap (Map.delete a m, r)

toListDMap (DMap (m,_)) = tail $ init $ Map.elems m
