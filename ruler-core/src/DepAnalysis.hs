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
import Debug.Trace(trace)

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

{-
-- performs the actual ordering
order :: (Ord k, Show k) => [k] -> [(a, k, [k])] -> [SCC (a, k, [k])]
order _ [] = []
order starts deps
  = sccs'
  where
    vals = map convert' sccs
    convert' (AcyclicSCC v) = [extract v]
    convert' (CyclicSCC vs) = map extract vs
  
    (graph, lookupVal,lookupVert) = graphFromEdges deps
    extract n = case lookupVal n of
                  (_,a,_) -> a

    verts  = vertices graph
    reach  = map (\v -> concatMap (reachable graph) (graph ! v)) verts
    pairs  = zip verts reach
    sccs   = cluster $ sortBy cmp' pairs
    sccs'  = map convert sccs
    
    convert (AcyclicSCC v) = AcyclicSCC (lookupVal v)
    convert (CyclicSCC vs) = CyclicSCC (map lookupVal vs)

    cluster []         = []
    cluster ((v,r):xs) = c : cluster zs
      where (ys,zs) = span (\(v', _) -> v' `elem` r) xs
            c | null ys && not (v `elem` r) = AcyclicSCC v
              | otherwise = CyclicSCC (v : map fst ys)
    
    cmp' (v1,r1) (v2,r2)
      = let z = cmp (v1,r1) (v2,r2)
        in trace (show (extract v1) ++ " ~ " ++ show (extract v2) ++ ": " ++ show z) z

    cmp (v1,r1) (v2,r2)
      | v1 == v2       = EQ
      | v2Bv1 && v1Bv2 = EQ
      | v1Bv2          = LT
      | v2Bv1          = GT
      | otherwise      = compare (extract v1) (extract v2)
      where
        v1Bv2 = v1 `elem` r2
        v2Bv1 = v2 `elem` r1
-}

{-
-- Returns the strongly-connected components in a total order
total :: [(Reason, DepItem, [DepItem])] -> [(Reason, DepItem, [DepItem])]
total deps = result
  where
    
    result = map (\(a,(r,bs)) -> (r,a,bs)) $ Map.assocs merged
    merged = Map.unionWith merge
               (Map.fromListWith merge [ (toVal a, (ReasonOrder, [toVal b])) | (a,b) <- G.edges gFinal ])
               (Map.fromList [ (a,(r,bs)) | (r,a,bs) <- deps ])
    
    merge (_,as) (r,bs) = (r,nub (as ++ bs))
    
    toVal x = G.lab' (G.context gFinal x)
    
    items = nub ([ s | (_,s,_) <- deps ] ++ [ d | (_,_,ds) <- deps, d <- ds ])
    (nodes, mp) = G.mkNodes G.new items
    (Just edges) = G.mkEdges mp [ (s,d,r) | (r, s, ds) <- deps, d <- ds ]
    
    gInitial :: G.Gr DepItem Reason
    gInitial = G.mkGraph nodes edges
    
    sinks = map G.node' $ G.gsel (null . G.suc') gInitial
    backwards = G.dfs sinks gInitial
    (_,gFinal) = foldl tighten (G.trc gInitial, gInitial) backwards
    
    tighten :: (G.Gr DepItem (), G.Gr DepItem Reason) -> G.Node -> (G.Gr DepItem (), G.Gr DepItem Reason)
    tighten (g, g') n
      = let ps    = G.suc gInitial n  -- successors in previous graph
            ps'   = sortBy compare' ps
            pairs = [ (a,b) | a <- ps', b <- ps', a /= b ]
        in foldr extend (g, g') pairs
    
    extend (a,b) (g, g')
      | not aBb && not bBa = case compare av bv of
                               EQ -> (g, g')
                               LT -> gBA
                               GT -> gAB
      | otherwise = (g, g')
      where
        (Just av) = G.lab g a
        (Just bv) = G.lab g b
      
        gAB = (G.trc $ G.insEdge (a,b,()) g, G.insEdge (a,b,ReasonOrder) g')
        gBA = (G.trc $ G.insEdge (b,a,()) g, G.insEdge (b,a,ReasonOrder) g')

        aBb = a `elem` G.suc g b
        bBa = b `elem` G.suc g a
    
    compare' a b
      = let (Just av) = G.lab gInitial a
            (Just bv) = G.lab gInitial b
        in compare av bv
-}

-- Refined approach
{-
total :: [(Reason, DepItem, [DepItem])] -> [(Reason, DepItem, [DepItem])]
total deps = result
  where
    result = map (\(a,(r,bs)) -> (r,a,bs)) $ Map.assocs merged
    merged = Map.unionWith merge
               (Map.fromListWith merge [ (toVal a, (ReasonOrder, [toVal b])) | (b,a) <- G.edges gFinal ])
               (Map.fromList [ (a,(r,bs)) | (r,a,bs) <- deps ])
    
    merge (_,as) (r,bs) = (r,nub (as ++ bs))
    toVal x = G.lab' (G.context gFinal x)
    
    items = nub ([ s | (_,s,_) <- deps ] ++ [ d | (_,_,ds) <- deps, d <- ds ])
    (nodes, mp) = G.mkNodes G.new items
    (Just edges) = G.mkEdges mp [ (s,d,r) | (r, s, ds) <- deps, d <- ds ]
    
    g0 :: G.Gr DepItem Reason
    g0 = G.mkGraph nodes edges
    g1 = removeCycles g0
    
    g2 :: G.Gr DepItem ()
    g2 = G.trc (G.grev g1)
    sinks = map G.node' $ G.gsel (null . G.suc') g2
    backwards = G.dfs sinks g2
    gFinal = g2 -- foldl tighten g2 backwards  -- note: graph is reversed compared to g0
    
    tighten :: G.Gr DepItem () -> G.Node -> G.Gr DepItem ()
    tighten g n
      = let ps    = G.suc g1 n  -- successors in older graph
            pairs = [ (a,b) | a <- ps, b <- ps, a /= b ]
        in foldr extend g pairs
    
    extend :: (G.Node, G.Node) -> G.Gr DepItem () -> G.Gr DepItem ()
    extend (a,b) g
      | not aBb && not bBa = case compare av bv of
                               EQ -> g
                               LT -> gBA
                               GT -> gAB
      | otherwise = g
      where
        (Just av) = G.lab g a
        (Just bv) = G.lab g b
        gAB = G.trc $ G.insEdge (b,a,()) g
        gBA = G.trc $ G.insEdge (a,b,()) g

        aBb = b `elem` G.suc g a
        bBa = a `elem` G.suc g b
    
removeCycles :: G.Gr DepItem Reason -> G.Gr DepItem Reason
removeCycles g0
  = g2
  where
    sources = map G.node' $ G.gsel (null . G.suc') g0
    forest = G.rdff sources g0
    g1 = G.efilter (const False) g0
    g2 = G.insEdges edges g1
    edges = getEdges Nothing forest
    
    getEdges parent = concatMap (getEdges' parent)
    getEdges' parent t
      = let front = case parent of
                      Nothing -> []
                      Just p  -> [(rootLabel t, p, ReasonOrder)]
        in front ++ getEdges (Just $ rootLabel t) (subForest t)
-}

-- totally new approach
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
    g1 = G.trc $ g0 -- removeCycles g0   -- search graph
    
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

removeCycles :: G.Gr DepItem Reason -> G.Gr DepItem Reason
removeCycles g0
  = g2
  where
    sources = map G.node' $ G.gsel (null . G.suc') g0
    forest = G.rdff sources g0
    g1 = G.efilter (const False) g0
    g2 = G.insEdges edges g1
    edges = getEdges Nothing forest
    
    getEdges parent = concatMap (getEdges' parent)
    getEdges' parent t
      = let front = case parent of
                      Nothing -> []
                      Just p  -> [(rootLabel t, p, ReasonOrder)]
        in front ++ getEdges (Just $ rootLabel t) (subForest t)

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
