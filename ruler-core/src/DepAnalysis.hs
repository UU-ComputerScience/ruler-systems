module DepAnalysis (analyze) where

import Common
import Data.Graph
import Data.Tree
import Data.Maybe
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.IntSet as IntSet
import Data.IntSet(IntSet)
import Data.List(partition, nub,sortBy,groupBy)
import Data.Array
import Debug.Trace(trace)

import qualified Data.Graph.Inductive as G


-- computes additional dependencies related to "late-as-possible" scheduling and avoidance of
-- cyclic visits
analyze :: [DepItem] -> [DepItem] -> [DepItem] -> Set DepItem -> [(Reason, DepItem, [DepItem])] -> [SCC DepItem]
analyze oSemStarts oStartVisits oEndVisits avoidVisits deps = map compToItem comps
  where
    comps = stronglyConnCompR $ total deps'

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

-- Returns the strongly-connected components in a total order
total :: [(Reason, DepItem, [DepItem])] -> [(Reason, DepItem, [DepItem])]
total deps = result
  where
    {-
    toSCC [x] | not (IntSet.member x directCycs) = AcyclicSCC (toVal x)
    toSCC xs = CyclicSCC (map toVal xs)
    
    components = G.scc gFinal
    directCycs = IntSet.fromList (map G.node' (G.gsel (\c -> G.node' c `elem` G.suc' c) gFinal))
    -}
    
    result = map (\(a,(r,bs)) -> (r,a,bs)) $ Map.assocs merged
    merged = Map.unionWith merge
               (Map.fromList [ (d, (ReasonOrder, [])) | (_, d) <- G.labNodes gFinal ])
               (Map.fromListWith merge [ (toVal a, (r, [toVal b])) | (a,b,r) <- G.labEdges gFinal ])
    
    merge (_,as) (r,bs) = (r,nub (as ++ bs))
    
    toVal x = G.lab' (G.context gFinal x)
    
    items = nub ([ s | (_,s,_) <- deps ] ++ [ d | (_,_,ds) <- deps, d <- ds ])
    (nodes, mp) = G.mkNodes G.new items
    (Just edges) = G.mkEdges mp [ (s,d,r) | (r, s, ds) <- deps, d <- ds ]
    
    gInitial :: G.Gr DepItem Reason
    gInitial = G.mkGraph nodes edges
    gFinal = foldr extend gInitial [ (a,b) | a <- nodes, b <- nodes ]
    
    extend ((a,av),(b,bv)) g
      | a == b = g
      | not aBb && not bBa = case compare av bv of
                               EQ -> g
                               LT -> gBA
                               GT -> gAB
      | otherwise = g
      where
        gAB = G.insEdge (a,b,ReasonOrder) g
        gBA = G.insEdge (b,a,ReasonOrder) g

        aBb = a `elem` G.reachable b g
        bBa = b `elem` G.reachable a g
