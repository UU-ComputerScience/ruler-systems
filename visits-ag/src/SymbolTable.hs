{-# OPTIONS -XTypeFamilies -XMultiParamTypeClasses -XFlexibleInstances -XOverlappingInstances
            -XEmptyDataDecls -XTypeOperators -XGADTs
            -XFunctionalDependencies -XFlexibleContexts -XUndecidableInstances -XIncoherentInstances
#-}
module SymbolTable( stmt,expr,defValue,value,assocs,tblGathEmpty,TblMsg(..),TblMsgs,End,TableGath,TableFin
                  , (:+),(:@),Tbl,(<!),scope,finalize,Path(..),ItemRef(..),InfoMap
                  , initialSpaces,Spaces,SpaceId(..),(.!),emptyPath,(.@@)
                  , catKeep,catSkip,(.@),skips,catSkip0,catSkip1,catSkip2,catSkip3,catSkip4
                  ) where

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
import Data.Graph


--
-- API
--

stmt :: Tabular e s t () -> TableGath e s t -> TableGath e s t
stmt m = fst . expr m

expr :: Typeable a => Tabular e s t a -> TableGath e s t -> (TableGath e s t, TableFin e s t -> a)
expr m (TableGath s)
  = (TableGath s', f)
  where
    n  = IntMap.size s
    w  = CompWrap m
    s' = IntMap.insert n w s
    f fin = lookupFin n fin

defValue :: (Categorical e s z, Ord s) => ItemRef e s t' a z -> SpaceRef s -> a -> e -> Tabular e s z ()
defValue = TabDefValue

value :: (Categorical e s z, Ord s) => ItemRef e s t' a z -> SpaceRef s -> a -> e -> Tabular e s z a
value = TabValue

assocs :: (Categorical e s z, Ord s) => Path e s (c :@ InfoMap e s k z a :+ r) z -> SpaceRef s -> Tabular e s z (Map k a)
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

class Categorical e s t where
  data Tbl t :: * -> * -> *
  emptyTbl :: Tbl t s e

instance Categorical e s End where
  data Tbl End s e = TblEnd
  emptyTbl = TblEnd

instance (Ord k, Categorical e s r, Categorical e s t) => Categorical e s (c :@ InfoMap e s k t a :+ r) where
  data Tbl (c :@ InfoMap e s k t a :+ r) s e = (Categorical e s t, Ord k) => TblCons (InfoMap e s k t a) (Tbl r s e)
  emptyTbl = TblCons (InfoMap Map.empty) emptyTbl

newtype InfoMap e s k t a = InfoMap (Map k (Descr e s t a))

data Descr e s t a = Descr !(Tbl t s e) !(SymTrie s (SymDef e a))

data SymDef e a
  = Undefined
  | Defined !a !Int !Int !(IntMap e) !IntSet

chaseSteps :: CatSelSteps e s m z -> Tbl z s e -> (Tbl m s e, Tbl m s e -> Tbl z s e, TblMsgs e)
chaseSteps None         tbl = (tbl, id, Seq.empty)
chaseSteps (Ref ref)    tbl = let (Descr tbl2 trie, f, msgs) = chaseRef ref tbl
                              in (tbl2, \tbl3 -> f (Descr tbl3 trie), msgs)
chaseSteps (Skip steps) tbl = let (tbl1, f, msgs) = chaseSteps steps tbl
                              in case tbl1 of
                                   TblCons mp tbl2 -> (tbl2, \tbl3 -> f (TblCons mp tbl3), msgs)

chaseRef :: ItemRef e s m a z -> Tbl z s e -> (Descr e s m a, Descr e s m a -> Tbl z s e, TblMsgs e)
chaseRef (Deref steps key info) tbl
  = let (tbl1, f, msgs) = chaseSteps steps tbl
    in case tbl1 of
         TblCons (InfoMap mp) tbl2 ->
           let descr = maybe (Descr emptyTbl emptyTrie) id (Map.lookup key mp)
           in ( descr
              , \tbl3 -> f (TblCons (InfoMap (Map.insert key tbl3 mp)) tbl2)
              , if undefDescr descr
                then msgs Seq.|> TblMsgMissing info
                else msgs
              )

derefSteps :: CatSelSteps e s m z -> Tbl z s e -> (Tbl m s e, TblMsgs e)
derefSteps None tbl         = (tbl, Seq.empty)
derefSteps (Skip steps) tbl = let (tbl1, msgs) = derefSteps steps tbl
                              in case tbl1 of
                                   TblCons _ tbl' -> (tbl', msgs)
derefSteps (Ref ref) tbl    = let (tbl1, msgs) = derefRef ref tbl
                              in case tbl1 of
                                   Descr tbl' _ -> (tbl', msgs)

derefRef :: ItemRef e s m a t -> Tbl t s e -> (Descr e s m a, TblMsgs e)
derefRef (Deref steps key info) tbl
  = let (tbl', msgs) = derefSteps steps tbl
    in case tbl' of
         TblCons (InfoMap mp) _ -> case Map.lookup key mp of
                                     Nothing -> (Descr emptyTbl emptyTrie, msgs Seq.|> TblMsgMissing info)
                                     Just d  -> (d, if undefDescr d then msgs Seq.|> TblMsgMissing info else msgs)

undefDescr :: Descr e s m a -> Bool
undefDescr (Descr _ trie)
  = not (any1 trie)
  where
    any1 (SymTrie mp)        = any any2 (IntMap.elems mp)
    any2 (TrieRoot mp v)     = any3 v || any any2 (Map.elems mp)
    any3 (Defined _ _ _ _ _) = True
    any3 Undefined           = False


--
-- Trie structure
--
-- Used for views upon the symbol table
--

newtype SymTrie s v
  = SymTrie (IntMap (Trie s v))

data Trie s v
  = TrieRoot (Map s (Trie s v)) v

emptyTrie :: SymTrie s v
emptyTrie = SymTrie IntMap.empty

chaseTrie :: Ord s => v -> SpaceRef s -> SymTrie s v -> (v, v -> SymTrie s v)
chaseTrie emptyv (SpaceRef n ks) (SymTrie mp)
  = (v, \v2 -> SymTrie (IntMap.insert n (f v2) mp))
  where
    root = IntMap.findWithDefault (TrieRoot Map.empty emptyv) n mp
    (v, f) = chaseScopes emptyv (reverse ks) root

chaseScopes :: Ord s => v -> [s] -> Trie s v -> (v, v -> Trie s v)
chaseScopes _ [] (TrieRoot mp v) = (v, \v2 -> TrieRoot mp v2)
chaseScopes emptyv (k:ks) (TrieRoot mp v)
  = (v2, \v3 -> TrieRoot (Map.insert k (f v3) mp) v)
  where
    root = Map.findWithDefault (TrieRoot Map.empty emptyv) k mp
    (v2, f) = chaseScopes emptyv ks root

trieDeref :: Ord s => v -> SpaceRef s -> SymTrie s v -> [v]
trieDeref emptyv (SpaceRef n ks) (SymTrie mp)
  = reverse $ chase (reverse ks) (IntMap.findWithDefault (TrieRoot Map.empty emptyv) n mp)
  where chase []     (TrieRoot _ v)  = [v]
        chase (k:ks) (TrieRoot mp v) = v : chase ks (Map.findWithDefault (TrieRoot Map.empty emptyv) k mp)


--
-- References
--

newtype Path e s t z = Path (CatSelSteps e s t z)

emptyPath :: Path e s z z
emptyPath = Path None

data CatSelSteps e s m z where
  None :: CatSelSteps e s m m
  Ref :: ItemRef e s t a z -> CatSelSteps e s t z
  Skip :: CatSelSteps e s (c :@ InfoMap e s k t a :+ r) z -> CatSelSteps e s r z

data ItemRef e s t a z where
  Deref :: Ord k => CatSelSteps e s (c :@ InfoMap e s k t a :+ r) z -> k -> e -> ItemRef e s t a z

infixl 5 .!
class Derefable v where
  type De v
  type Ds v
  type Dt v
  type Da v
  type Dk v
  type Dz v
  (.!) :: v -> (Dk v, De v) -> ItemRef (De v) (Ds v) (Dt v) (Da v) (Dz v)

instance Ord k => Derefable (Path e s (c :@ InfoMap e s k t a :+ r) z) where
  type De (Path e s (c :@ InfoMap e s k t a :+ r) z) = e
  type Ds (Path e s (c :@ InfoMap e s k t a :+ r) z) = s
  type Dt (Path e s (c :@ InfoMap e s k t a :+ r) z) = t
  type Da (Path e s (c :@ InfoMap e s k t a :+ r) z) = a
  type Dk (Path e s (c :@ InfoMap e s k t a :+ r) z) = k
  type Dz (Path e s (c :@ InfoMap e s k t a :+ r) z) = z
  (.!) (Path steps) (k, info) = Deref steps k info

instance Ord k => Derefable (ItemRef e s (c :@ InfoMap e s k t a :+ r) a' z) where
  type De (ItemRef e s (c :@ InfoMap e s k t a :+ r) a' z) = e
  type Ds (ItemRef e s (c :@ InfoMap e s k t a :+ r) a' z) = s
  type Dt (ItemRef e s (c :@ InfoMap e s k t a :+ r) a' z) = t
  type Da (ItemRef e s (c :@ InfoMap e s k t a :+ r) a' z) = a
  type Dk (ItemRef e s (c :@ InfoMap e s k t a :+ r) a' z) = k
  type Dz (ItemRef e s (c :@ InfoMap e s k t a :+ r) a' z) = z
  (.!) ref (k, info) = Deref (Ref ref) k info

infix 6 .@
infix 6 .@@

class Selectable v where
  type Se v
  type Ss v
  type St v
  type Sz v
  (.@@) :: v -> (Path (Se v) (Ss v) (St v) (Sz v) -> Path (Se v) (Ss v) t' (Sz v)) -> Path (Se v) (Ss v) t' (Sz v)
  (.@) :: (Selectable2 (Se v) (Ss v) c (St v) t') => v -> c -> Path (Se v) (Ss v) t' (Sz v)
  (.@) x c = x .@@ skips c

instance Selectable (Path e s t z) where
  type Se (Path e s t z) = e
  type Ss (Path e s t z) = s
  type St (Path e s t z) = t
  type Sz (Path e s t z) = z
  (.@@) p f = f p

instance Selectable (ItemRef e s t a z) where
  type Se (ItemRef e s t a z) = e
  type Ss (ItemRef e s t a z) = s
  type St (ItemRef e s t a z) = t
  type Sz (ItemRef e s t a z) = z
  (.@@) ref f = f (Path (Ref ref))

catKeep :: Path e s t z -> Path e s t z
catKeep = id

catSkip :: Path e s ((c :@ InfoMap e s k t a) :+ r) z -> Path e s r z
catSkip (Path steps) = Path (Skip steps)

catSkip0 = catKeep
catSkip1 = catSkip
catSkip2 = catSkip . catSkip
catSkip3 = catSkip2 . catSkip
catSkip4 = catSkip3 . catSkip

class Selectable2 e s c t t' | c t -> t', t -> e s, t' -> e s where
  skips :: c -> Path e s t z -> Path e s t' z

instance Selectable2 e s c (c :@ InfoMap e s k t a :+ r) (c :@ InfoMap e s k t a :+ r) where
  skips _ = catKeep

instance Selectable2 e s c r t => Selectable2 e s c (c' :@ InfoMap e s k t' a :+ r) t where
  skips c p = skips c (catSkip p)


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
data SpaceRef k where
 SpaceRef :: Int -> [k] -> SpaceRef k

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
(<!) :: (Space s t') => Spaces t' k -> SpaceId s -> SpaceRef k
sps <! sid = SpaceRef (spConses sid sps) (spLookup sid sps)

scope :: Space s t => k -> SpaceId s -> Spaces t k -> Spaces t k
scope = spInsert


--
-- Computations
--

data Tabular e s z a where
  TabFinish    :: a -> Tabular e s z a
  TabBind      :: Tabular e s z a' -> (a' -> Tabular e s z a) -> Tabular e s z a
  TabValue     :: (Categorical e s z, Ord s) => ItemRef e s t' a z -> SpaceRef s -> a -> e -> Tabular e s z a
  TabAssocs    :: (Categorical e s z, Ord s) => Path e s (c :@ InfoMap e s k z a :+ r) z -> SpaceRef s -> Tabular e s z (Map k a)
  TabDefValue  :: (Categorical e s z, Ord s) => ItemRef e s t' a z -> SpaceRef s -> a -> e -> Tabular e s z ()
  TabDefAssocs :: (Categorical e s z, Ord s) => Path e s (c :@ InfoMap e s k z a :+ r) z -> SpaceRef s -> Map k a -> e -> Tabular e s z ()

instance Monad (Tabular e s z) where
  (>>=)  = TabBind
  return = TabFinish

data CompWrap e s z where
  CompWrap :: Typeable a => Tabular e s z a -> CompWrap e s z

newtype TableGath e s z = TableGath (IntMap (CompWrap e s z))


--
-- Evaluator
--

data EvalState e s z = EvalState { stId        :: Int          -- id-nr of the computation
                                 , stNewStamp  :: Int          -- the time stamp of the current iteration
                                 , stReadStamp :: Int          -- most recent timestamp encountered so far
                                 , stTbl       :: Tbl z s e    -- the table so far
                                 , stMsgs      :: TblMsgs e    -- messages (i.e. errors) during evaluation
                                 , stReach     :: IntSet       -- computations we depend on so far
                                 , stChanged   :: Bool         -- wether or not we changed an entry
                                 }
type Eval e s z a = State (EvalState e s z) a

appendMsg :: TblMsg e -> Eval e s z ()
appendMsg msg
  = modify (\s -> s { stMsgs = stMsgs s Seq.|> msg })

appendMsgs :: TblMsgs e -> Eval e s z ()
appendMsgs msgs
  = modify (\s -> s { stMsgs = stMsgs s Seq.>< msgs })

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
  = do tbl <- gets stTbl
       let (Descr _ trie, msgs) = derefRef ref tbl
       appendMsgs msgs
       chase (trieDeref Undefined sp trie)
  where
       chase []     = do appendMsg (TblMsgMissing e)
                         return a
       chase (d:ds) = case d of
                        Undefined -> chase ds
                        Defined a evalId stamp _ ids -> do encounterIds (IntSet.insert evalId ids)
                                                           encounterStamp stamp
                                                           return a
eval (TabAssocs (Path path) sp)
  = do tbl <- gets stTbl
       let (tbl', msgs) = derefSteps path tbl
       appendMsgs msgs
       case tbl' of
         TblCons (InfoMap mp) _
           -> do maps <- sequence [ chase k (trieDeref Undefined sp trie) | (k, Descr _ trie) <- Map.assocs mp ]
                 return (Map.unions maps)
  where
    chase _ []             = return Map.empty
    chase k (Undefined:ds) = chase k ds
    chase k (Defined a evalId stamp _ ids : _)
      = do encounterIds (IntSet.insert evalId ids)
           encounterStamp stamp
           return (Map.singleton k a)
eval (TabDefValue ref sp a e)
  = do tbl    <- gets stTbl
       stamp  <- gets stReadStamp
       evalId <- gets stId
       idsCur <- gets stReach

       let (descr, tblF, msgs) = chaseRef ref tbl
       appendMsgs msgs
       case descr of
         Descr tbl' trie ->
           let (def, trieF) = chaseTrie Undefined sp trie
           in case def of
                Undefined -> let occs   = IntMap.singleton evalId e
                                 descr' = Descr tbl' (trieF (Defined a evalId stamp occs idsCur))
                             in modify (\s -> s { stTbl = tblF descr', stChanged = True })
                Defined a' evalId' stamp' occs ids
                  -> let ids'  = idsCur `IntSet.union` ids
                         occs' = IntMap.insert evalId e occs
                     in if evalId `IntSet.member` ids'
                        then appendMsg (TblMsgCyclic e)
                        else if evalId /= evalId'
                             then let descr' = Descr tbl' (trieF (Defined a' evalId' stamp' occs' ids'))
                                  in modify (\s -> s { stTbl = tblF descr' })
                             else do when (IntMap.size occs > 1) $
                                       appendMsg (TblMsgDuplicate $ IntMap.elems occs)
                                     if stamp' >= stamp
                                      then return ()  -- already up-to-date
                                      else let descr' = Descr tbl' (trieF (Defined a evalId stamp occs' ids'))
                                           in modify (\s -> s { stTbl = tblF descr', stChanged = True })


--
-- Finalized result
--

newtype TableFin e s t = TableFin (IntMap CompResult)
data CompResult where
  CompResult :: Typeable a => a -> CompResult

finalize :: Categorical e s t => TableGath e s t -> (TableFin e s t, TblMsgs e)
finalize (TableGath compSet)
  = (TableFin (psRes finalstate), psMsgs finalstate)
  where
    initstate  = PassState { psStamp = 1, psChanged = False, psTbl = emptyTbl, psRes = IntMap.empty
                           , psMsgs = Seq.empty, psDeps = IntMap.map (\v -> (v, IntSet.empty)) compSet }
    finalstate = execState run initstate
    run = do onepass
             changed <- gets psChanged
             if changed
              then do modify (\ps -> ps { psStamp = psStamp ps + 1, psChanged = False, psMsgs = Seq.empty } )
                      run
              else return ()

data PassState e s z = PassState { psStamp :: Int, psChanged :: Bool, psTbl :: Tbl z s e
                                 , psRes :: IntMap CompResult, psMsgs :: TblMsgs e, psDeps :: IntMap (CompWrap e s z, IntSet) }

onepass :: State (PassState e s z) ()
onepass = do deps <- gets psDeps
             let deps' = flattenSCCs (stronglyConnComp [((k,w),k,IntSet.toList ks) | (k,(w,ks)) <- IntMap.assocs deps ])
             mapM_ (uncurry execOne) deps'

execOne :: Int -> CompWrap e s z -> State (PassState e s z) ()
execOne evalId w@(CompWrap expr)
  = modify (\ps -> let st  = EvalState { stId = evalId, stNewStamp = psStamp ps, stReadStamp = 0, stTbl = psTbl ps
                                       , stMsgs = Seq.empty, stReach = IntSet.empty, stChanged = False }
                       (a, st') = runState (eval expr) st
                   in ps { psChanged = psChanged ps || stChanged st'
                         , psTbl     = stTbl st'
                         , psRes     = IntMap.insert evalId (CompResult a) (psRes ps)
                         , psDeps    = IntMap.insert evalId (w, stReach st') (psDeps ps)
                         , psMsgs    = psMsgs ps Seq.>< (Seq.take 1 (stMsgs st'))  -- max one error per 'evalId'
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
