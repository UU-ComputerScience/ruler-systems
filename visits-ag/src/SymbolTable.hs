{-# OPTIONS -XTypeFamilies  -XMultiParamTypeClasses -XFunctionalDependencies
            -XOverlappingInstances -XUndecidableInstances -XFlexibleInstances -XIncoherentInstances -XFlexibleContexts
            -XEmptyDataDecls -XTypeOperators -XGADTs #-}
module SymbolTable where

import Data.IntSet(IntSet)
import qualified Data.IntSet as IntSet
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap
import Data.Sequence(Seq)
import qualified Data.Sequence as Seq
import Data.Monoid
import Control.Monad.State.Strict
import Data.Typeable


--
-- API
--

apply :: Typeable a => Tabular e s t a -> TableGath e s t -> (TableGath e s t, TableFin e s t -> a)
apply m (TableGath s)
  = (TableGath s', f)
  where
    n  = IntMap.size s
    w  = CompWrap m
    s' = IntMap.insert n w s
    f fin = lookupFin n fin

defValue :: (Categorical e z, Ord s) => ItemRef e t' a z -> SpaceRef s z -> a -> e -> Tabular e s z ()
defValue = TabDefValue

value :: (Categorical e z, Ord s) => ItemRef e t' a z -> SpaceRef s z -> a -> e -> Tabular e s z a
value = TabValue

assocs :: (Categorical e z, Ord s) => Path e (c :@ InfoMap e k z a :+ r) z -> SpaceRef s z -> Tabular e s z (Map k a)
assocs = TabAssocs

tblGathEmpty :: TableGath e s t
tblGathEmpty = TableGath IntMap.empty

type TblMsgs e = Seq (TblMsg e)

data TblMsg e
  = TblMsgDuplicate [e]
  | TblMsgMissing e
  | TblMsgCyclic e

instance Show e => Show (TblMsg e) where
  show (TblMsgDuplicate es) = "duplicate items: " ++ show es
  show (TblMsgMissing e)    = "missing item: " ++ show e
  show (TblMsgCyclic e)     = "cyclic definition for: " ++ show e


--
-- Symbol-table data structure
--

data End
data (:+) a b
data (:@) a b
infixr 4 :+
infix 6 :@

class Categorical e t where
  data Tbl t :: * -> *
  emptyTbl :: Tbl t e

instance Categorical e End where
  data Tbl End e = TblEnd
  emptyTbl = TblEnd

instance (Ord k, Categorical e r, Categorical e t) => Categorical e (c :@ InfoMap e k t a :+ r) where
  data Tbl (c :@ InfoMap e k t a :+ r) e = (Categorical e t, Ord k) => TblCons (InfoMap e k t a) (Tbl r e)
  emptyTbl = TblCons (InfoMap Map.empty) emptyTbl

newtype InfoMap e k t a = InfoMap (Map k (Descr e t a))

data Descr e t a = Descr !(Tbl t e) !(Status e a) !IntSet

data Status e a
  = Missing
  | Defined !a !Int !Int !(IntMap e)

chaseSteps :: CatSelSteps e m z -> Tbl z e -> (Tbl m e, Tbl m e -> Tbl z e)
chaseSteps None         tbl = (tbl, id)
chaseSteps (Ref ref)    tbl = let (Descr tbl2 val origins, f) = chaseRef ref tbl
                              in (tbl2, \tbl3 -> f (Descr tbl3 val origins))
chaseSteps (Skip steps) tbl = let (tbl1, f) = chaseSteps steps tbl
                              in case tbl1 of
                                   TblCons mp tbl2 -> (tbl2, \tbl3 -> f (TblCons mp tbl3))

chaseRef :: ItemRef e m a z -> Tbl z e -> (Descr e m a, Descr e m a -> Tbl z e)
chaseRef (Deref steps key) tbl
  = let (tbl1, f) = chaseSteps steps tbl
    in case tbl1 of
         TblCons (InfoMap mp) tbl2 -> ( maybe (Descr emptyTbl Missing IntSet.empty) id (Map.lookup key mp)
                                      , \tbl3 -> f (TblCons (InfoMap (Map.insert key tbl3 mp)) tbl2)
                                      )

derefSteps :: CatSelSteps e m z -> Tbl z e -> Tbl m e
derefSteps None tbl         = tbl
derefSteps (Skip steps) tbl = case derefSteps steps tbl of
                                TblCons _ tbl' -> tbl'
derefSteps (Ref ref) tbl    = case derefRef ref tbl of
                                Descr tbl' _ _ -> tbl'

derefRef :: ItemRef e m a t -> Tbl t e -> Descr e m a
derefRef (Deref steps key) tbl
  = case derefSteps steps tbl of
      TblCons (InfoMap mp) _ -> maybe (Descr emptyTbl Missing IntSet.empty) id (Map.lookup key mp)

--
-- Trie structure
--

newtype SymTrie e s z
  = SymTrie (IntMap (Trie s (Tbl z e)))

data Trie s v
  = TrieRoot (Map s (Trie s v)) v

emptyTrie :: SymTrie e s z
emptyTrie = SymTrie IntMap.empty

chaseTrie :: (Ord s, Categorical e z) => SpaceRef s z -> SymTrie e s z -> (Tbl z e, Tbl z e -> SymTrie e s z)
chaseTrie (SpaceRef n ks) (SymTrie mp)
  = (tbl, \tbl2 -> SymTrie (IntMap.insert n (f tbl2) mp))
  where
    root = IntMap.findWithDefault (TrieRoot Map.empty emptyTbl) n mp
    (tbl, f) = chaseScopes (reverse ks) root

chaseScopes :: (Ord s, Categorical e z) => [s] -> Trie s (Tbl z e) -> (Tbl z e, Tbl z e -> Trie s (Tbl z e))
chaseScopes [] (TrieRoot mp tbl) = (tbl, \tbl2 -> TrieRoot mp tbl2)
chaseScopes (k:ks) (TrieRoot mp tbl)
  = (tbl2, \tbl3 -> TrieRoot (Map.insert k (f tbl3) mp) tbl)
  where
    root = Map.findWithDefault (TrieRoot Map.empty emptyTbl) k mp
    (tbl2, f) = chaseScopes ks root

trieDeref :: (Ord s, Categorical e z) => SpaceRef s z -> SymTrie e s z -> [Tbl z e]
trieDeref (SpaceRef n ks) (SymTrie mp)
  = reverse $ chase (reverse ks) (IntMap.findWithDefault (TrieRoot Map.empty emptyTbl) n mp)
  where chase []     (TrieRoot _ tbl)  = [tbl]
        chase (k:ks) (TrieRoot mp tbl) = tbl : chase ks (Map.findWithDefault (TrieRoot Map.empty emptyTbl) k mp)


--
-- References
--

newtype Path e t z = Path (CatSelSteps e t z)

data CatSelSteps e m z where
  None :: CatSelSteps e m m
  Ref :: ItemRef e t a z -> CatSelSteps e t z
  Skip :: CatSelSteps e (c :@ InfoMap e k t a :+ r) z -> CatSelSteps e r z

data ItemRef e t a z where
  Deref :: Ord k => CatSelSteps e (c :@ InfoMap e k t a :+ r) z -> k -> ItemRef e t a z

infixl 5 .!
class Derefable e v t k a | v -> e k t a where
  (.!) :: v z -> k -> ItemRef e t a z

instance Ord k => Derefable e (Path e (c :@ InfoMap e k t a :+ r)) t k a where
  (.!) (Path steps) = Deref steps

instance Ord k => Derefable e (ItemRef e (c :@ InfoMap e k t a :+ r) a') t k a where
  (.!) ref = Deref (Ref ref)

infix 6 .@
class CatSelectable e v c t | v c -> t, v -> e where
  (.@) :: v z -> c -> Path e t z

instance CatSelectable e (Path e (c :@ InfoMap e k t a :+ r)) c (c :@ InfoMap e k t a :+ r) where
  (.@) p _ = p

instance (CatSelectable e (Path e r) c t, m ~ InfoMap e k t a) => CatSelectable e (Path e (c' :@ m :+ r)) c t where
  (.@) (Path steps) c = Path (Skip steps) .@ c

instance CatSelectable e (Path e t) c z => CatSelectable e (ItemRef e t a) c z where
  (.@) ref c = Path (Ref ref) .@ c

{-
data RefsComp t t' a a' where
  RefsEq :: RefsComp t t a a
  RefsLt :: RefsComp t t' a a'
  RefsGt :: RefsComp t t' a a'

data StepsComp t t' where
  StepsEq :: StepsComp t t
  StepsLt :: StepsComp t t'
  StepsGt :: StepsComp t t'

compareSteps :: CatSelSteps m z -> CatSelSteps m' z -> StepsComp m m'
compareSteps None None = StepsEq
compareSteps None _    = StepsLt
compareSteps _ None    = StepsGt
compareSteps (Ref ref1) (Ref ref2)
  = case compareRefs ref1 ref2 of
      RefsLt -> StepsLt
      RefsEq -> StepsEq
      RefsGt -> StepsGt
compareSteps (Ref _) _ = StepsLt
compareSteps _ (Ref _) = StepsGt
compareSteps (Skip steps1) (Skip steps2)
  = case compareSteps steps1 steps2 of
      StepsLt -> StepsLt
      StepsEq -> StepsEq
      StepsGt -> StepsGt

compareRefs :: ItemRef t a z -> ItemRef t' a' z -> RefsComp t t' a a'
compareRefs (Deref steps1 k1) (Deref steps2 k2)
  = case compareSteps steps1 steps2 of
      StepsLt -> RefsLt
      StepsEq -> case compare k1 k2 of
                   LT -> RefsLt
                   EQ -> RefsEq
                   GT -> RefsGt
      StepsGt -> RefsGt

type WaitSet z = Set.Set (WaitItem z)

data WaitItem z where
  WaitRef  :: ItemRef t a z -> WaitItem z
  WaitPath :: Path t z -> WaitItem z

instance Eq (WaitItem z) where
  (WaitRef ref1) == (WaitRef ref2)
     = case compareRefs ref1 ref2 of
         RefsEq -> True
         _      -> False
  (WaitPath (Path path1)) == (WaitPath (Path path2))
    = case compareSteps path1 path2 of
        StepsEq -> True
        _       -> False
  _ == _ = False

instance Ord (WaitItem z) where
  compare (WaitRef ref1) (WaitRef ref2)
    = case compareRefs ref1 ref2 of
        RefsLt -> LT
        RefsEq -> EQ
        RefsGt -> GT
  compare (WaitPath (Path path1)) (WaitPath (Path path2))
    = case compareSteps path1 path2 of
        StepsLt -> LT
        StepsEq -> EQ
        StepsGt -> GT
  compare (WaitPath _) (WaitRef _) = LT
  compare (WaitRef _) (WaitPath _) = GT
-}

--
-- Name spaces
--

class Namespaces t where
  data Spaces t :: * -> *
  initialSpaces :: Ord k => Spaces t k

instance Namespaces End where
  data Spaces End k = SpacesNil
  initialSpaces = SpacesNil

instance Namespaces r => Namespaces (t :+ r) where
  data Spaces (t :+ r) k = SpacesCons [k] (Spaces r k)
  initialSpaces = SpacesCons [] initialSpaces

data SpaceId s = SpaceId
data SpaceRef k t where
 SpaceRef :: Int -> [k] -> SpaceRef k t

class Space s t where
  spInsert :: k -> SpaceId s -> Spaces t k -> Spaces t k
  spLookup :: SpaceId s -> Spaces t k -> [k]
  spConses :: SpaceId s -> Spaces t k -> Int

instance Space s (s :+ r) where
  spInsert k _ (SpacesCons ks r) = SpacesCons (k:ks) r
  spLookup _ (SpacesCons ks _)   = ks
  spConses _ _                   = 1

instance Space s r => Space s (s' :+ r) where
  spInsert k s (SpacesCons ks r) = SpacesCons ks (spInsert k s r)
  spLookup s (SpacesCons _ r)    = spLookup s r
  spConses s (SpacesCons _ r)    = 1 + spConses s r

infix 6 <!
(<!) :: (Space s t') => Spaces t' k -> SpaceId s -> SpaceRef k t
sps <! sid = SpaceRef (spConses sid sps) (spLookup sid sps)

scope :: Space s t => k -> SpaceId s -> Spaces t k -> Spaces t k
scope = spInsert


--
-- Computations
--

data Tabular e s z a where
  TabFinish    :: a -> Tabular e s z a
  TabBind      :: Tabular e s z a' -> (a' -> Tabular e s z a) -> Tabular e s z a
  TabValue     :: (Categorical e z, Ord s) => ItemRef e t' a z -> SpaceRef s z -> a -> e -> Tabular e s z a
  TabAssocs    :: (Categorical e z, Ord s) => Path e (c :@ InfoMap e k z a :+ r) z -> SpaceRef s z -> Tabular e s z (Map k a)
  TabDefValue  :: (Categorical e z, Ord s) => ItemRef e t' a z -> SpaceRef s z -> a -> e -> Tabular e s z ()
  TabDefAssocs :: (Categorical e z, Ord s) => Path e (c :@ InfoMap e k z a :+ r) z -> SpaceRef s z -> Map k a -> e -> Tabular e s z ()

instance Monad (Tabular e s z) where
  (>>=)  = TabBind
  return = TabFinish

data CompWrap e s z where
  CompWrap :: Typeable a => Tabular e s z a -> CompWrap e s z

newtype TableGath e s z = TableGath (IntMap (CompWrap e s z))


--
-- Evaluator
--

data EvalState e s z = EvalState { stId :: Int              -- id-nr of the computation
                                 , stNewStamp :: Int        -- the time stamp of the current iteration
                                 , stReadStamp :: Int       -- most recent timestamp encountered so far
                                 , stTrie :: SymTrie e s z  -- the trie so far
                                 , stMsgs :: TblMsgs e      -- messages (i.e. errors) during evaluation
                                 , stReach :: IntSet        -- computations we depend on so far
                                 , stChanged :: Bool        -- wether or not we changed an entry
                                 }
type Eval e s z a = State (EvalState e s z) a

appendMsg :: TblMsg e -> Eval e s z ()
appendMsg msg
  = modify (\s -> s { stMsgs = stMsgs s Seq.|> msg })

encounterIds :: IntSet -> Eval e s z ()
encounterIds set
  = modify (\s -> s {  stReach = set `IntSet.union` stReach s })

encounterStamp :: Int -> Eval e s z ()
encounterStamp prev
  = modify (\s -> s { stReadStamp = prev `max` stReadStamp s })

eval :: Tabular e s z a -> Eval e s z a
eval (TabFinish v) = return v
eval (TabBind m f)
  = do a <- eval m
       eval (f a)
eval (TabValue ref sp a e)
  = do trie <- gets stTrie
       chase (trieDeref sp trie)
  where
    chase [] = do appendMsg (TblMsgMissing e)
                  return a
    chase (tbl:tbls)
      = case derefRef ref tbl of
          Descr _ Missing _                 -> chase tbls
          Descr _ (Defined a _ stamp _) ids -> do encounterIds ids
                                                  encounterStamp stamp
                                                  return a
eval (TabAssocs (Path path) sp)
  = do trie <- gets stTrie
       chase (trieDeref sp trie)
  where
    chase [] = return Map.empty
    chase (tbl:tbls)
      = do mp <- chase tbls
           case derefSteps path tbl of
             TblCons (InfoMap mp1) _
               -> let (mp2, ids, stamp) = foldr (\(k,a,ids,s1) (mp,ids',s) -> (Map.insert k a mp, ids `IntSet.union` ids', s1 `max` s))
                                                (mp,IntSet.empty, 0)
                                                [(k,a,ids,s) | (k, Descr _ (Defined a _ s _) ids) <- Map.assocs mp1]
                  in do encounterIds ids
                        encounterStamp stamp
                        return mp2
eval (TabDefValue ref sp a e)
  = do trie   <- gets stTrie
       stamp  <- gets stReadStamp
       evalId <- gets stId
       idsCur <- gets stReach

       let (tbl, trieF)  = chaseTrie sp trie
           (descr, tblF) = chaseRef ref tbl

       case descr of
         Descr tbl status ids ->
           let status1 = case status of
                           Missing -> Defined a evalId stamp (IntMap.singleton evalId e)
                           _       -> status
               ids' = idsCur `IntSet.union` ids
           in if evalId `IntSet.member` ids'
              then appendMsg (TblMsgCyclic e)
              else case status1 of
                     Defined a evalId' stamp' occs ->
                       do when (IntMap.size occs > 1) $
                            appendMsg (TblMsgDuplicate $ IntMap.elems occs)
                          if evalId /= evalId'
                           then let descr' = Descr tbl (Defined a evalId' stamp' (IntMap.insert evalId e occs))
                                                       (idsCur `IntSet.union` ids)
                                in modify (\s -> s{ stTrie = trieF $ tblF descr' })
                           else if stamp' >= stamp
                                then return () -- already up to date
                                else let descr' = Descr tbl (Defined a evalId stamp occs) (idsCur `IntSet.union` ids)
                                     in modify (\s -> s{ stTrie = trieF $ tblF descr', stChanged = True })


--
-- Finalized result
--

newtype TableFin e s t = TableFin (IntMap CompResult)
data CompResult where
  CompResult :: Typeable a => a -> CompResult

finalize :: TableGath e s t -> (TableFin e s t, TblMsgs e)
finalize (TableGath compSet)
  = (TableFin (psRes finalstate), psMsgs finalstate)
  where
    comps = IntMap.assocs compSet
    initstate  = PassState { psStamp = 1, psChanged = False, psTrie = emptyTrie, psRes = IntMap.empty, psMsgs = Seq.empty }
    finalstate = execState run initstate
    run = do onepass comps
             changed <- gets psChanged
             if changed
              then do modify (\ps -> ps { psStamp = psStamp ps + 1, psChanged = False, psMsgs = Seq.empty } )
                      run
              else return ()

data PassState e s z = PassState { psStamp :: Int, psChanged :: Bool, psTrie :: SymTrie e s z
                                 , psRes :: IntMap CompResult, psMsgs :: TblMsgs e }

onepass :: [(Int, CompWrap e s z)] -> State (PassState e s z) ()
onepass = mapM_ execOne

execOne :: (Int, CompWrap e s z) -> State (PassState e s z) ()
execOne (evalId, CompWrap expr)
  = modify (\ps -> let st  = EvalState { stId = evalId, stNewStamp = psStamp ps, stReadStamp = 0, stTrie = psTrie ps
                                       , stMsgs = Seq.empty, stReach = IntSet.empty, stChanged = False }
                       (a, st') = runState (eval expr) st
                   in ps { psChanged = psChanged ps || stChanged st'
                         , psTrie    = stTrie st'
                         , psRes     = IntMap.insert evalId (CompResult a) (psRes ps)
                         })

lookupFin :: Typeable a => Int -> TableFin e s t -> a
lookupFin key (TableFin mp)
  = case IntMap.lookup key mp of
      Nothing             -> error "lookupFin: key is not in map"
      Just (CompResult x) -> case cast x of
                               Nothing -> error "lookupFin: item is of the incorrect type"
                               Just a  -> a

instance Show (TableFin e s t) where
  show (TableFin mp) = show $ IntMap.keys mp
