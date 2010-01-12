{-# OPTIONS -XGADTs #-}
module Merger where


import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq)
import Data.Monoid
import Data.List(intercalate, nub)


--
-- Typed representation of trees where subtrees are identified by their names k
--

newtype AbsMap e k a = AbsMap ( Map k (Val e k a)  -- the map
                              , Maybe (Val e k a)  -- possibly default value
                              )

data Val e k a
  = Val (Chain e k a) (Occurrence e)

newtype ChnI a = ChnI a
newtype ChnS a = ChnS a
newtype ChnT   = ChnT ()

data Chain e k a where
  ChnItem :: Overridable e a -> Chain e k r -> Chain e k (ChnI a, r)
  ChnSub  :: AbsMap e k a -> Chain e k r -> Chain e k (ChnS a, r)
  ChnEnd  :: Chain e k ChnT

data Overridable e a where
  Replaceable :: a -> Overridable e a
  Appendable  :: Monoid a => a -> Overridable e a
  Fixed       :: Eq a => a -> [e] -> Overridable e a
  Absent      :: e -> Overridable e a

data Occurrence e
  = SingleDef [e]
  | MultDef [e]
  | OccRequire e

data MergeInfo e
  = Dup [e]
  | Missing e


--
-- Show instances
--

instance Show k => Show (AbsMap e k a) where
  show (AbsMap (mp, Nothing))  = "{" ++ showMp mp ++ "}"
  show (AbsMap (mp, Just occ)) = "{" ++ showMp mp ++ "; " ++ show occ ++ "}"

showMp :: Show k => (Map k (Val e k a)) -> String
showMp mp = intercalate ", " (map (\(k,v) -> show k ++ " -> " ++ show v) $ Map.toList mp)

instance Show k => Show (Val e k a) where
  show (Val chn occ) = show occ ++ " " ++ showChn chn

showChn :: Show k => Chain e k a -> String
showChn = intercalate ".". showChnAsList

showChnAsList :: Show k => Chain e k a -> [String]
showChnAsList (ChnItem ov chn) = show ov : showChnAsList chn
showChnAsList (ChnSub mp chn)  = show mp : showChnAsList chn
showChnAsList _                = []

instance Show (Overridable e a) where
  show (Replaceable _) = "val-repl"
  show (Appendable _)  = "val-multi"
  show (Fixed _ _)     = "val-fixed"
  show (Absent _)      = "val-null"

instance Show (Occurrence e) where
  show (SingleDef _)  = "single"
  show (MultDef [])   = "def-null"
  show (MultDef _)    = "def-many"
  show (OccRequire _) = "req"


--
-- Monoid instances
--


instance Ord k => Monoid (AbsMap e k a) where
  mempty = AbsMap (Map.empty, Nothing)
  mappend (AbsMap (mpl, defl)) (AbsMap (mpr, defr)) = AbsMap (Map.unionWith mappend mpl mpr, defl `mappend` defr)

singleton :: Ord k => k -> Val e k a -> AbsMap e k a
singleton k v = AbsMap (Map.singleton k v, Nothing)

dflt :: Ord k => Val e k a -> AbsMap e k a
dflt v = AbsMap (Map.empty, Just v)

many :: Ord k => [k] -> (Maybe k -> Val e k a) -> AbsMap e k a
many [] f = AbsMap (Map.empty, Just $ f Nothing)
many ks f = many1 ks (\x -> f (Just x))

many1 :: Ord k => [k] -> (k -> Val e k a) -> AbsMap e k a
many1 ks f = AbsMap (Map.fromList (map (\k -> (k,f k)) ks), Nothing)

instance Ord k => Monoid (Val e k a) where
  mempty = Val mempty mempty
  mappend (Val chnl el) (Val chnr er) = Val (chnl `mappend` chnr) (el `mappend` er)

instance Ord k => Monoid (Chain e k a) where
  mempty = error "mempty on chains is not supported"
  mappend (ChnItem ovl chnl) (ChnItem ovr chnr) = ChnItem (ovl `mappend` ovr) (chnl `mappend` chnr)
  mappend (ChnSub mpl chnl) (ChnSub mpr chnr)   = ChnSub  (mpl `mappend` mpr) (chnl `mappend` chnr)
  mappend ChnEnd ChnEnd                         = ChnEnd

instance Monoid (Overridable e a) where
  mempty = error "mempty on overridables is not supported"
  mappend (Absent _) o = o
  mappend o (Absent _) = o
  mappend (Replaceable _) o = o
  mappend o (Replaceable _) = o
  mappend (Appendable a1) (Appendable a2) = Appendable (a1 `mappend` a2)
  mappend (Fixed al el) (Fixed ar er)
    | length el + length er <= 2 && al == ar = Fixed al el
    | otherwise                              = Fixed al (el ++ er)

instance Monoid (Occurrence e) where
  mempty = error "mempty on occurrences is not supported"
  mappend (OccRequire _) o = o
  mappend o (OccRequire _) = o
  mappend (SingleDef esl) (SingleDef esr) = SingleDef (esl ++ esr)
  mappend (SingleDef esl) (MultDef esr)   = SingleDef (esl ++ esr)
  mappend (MultDef esl) (SingleDef esr)   = SingleDef (esl ++ esr)
  mappend (MultDef esl) (MultDef esr)     = MultDef (esl ++ esr)


--
-- resolve the duplicate or missing entries in the map
--

resolve :: (Eq e, Ord k) => AbsMap e k a -> (AbsMap e k a, [MergeInfo e])
resolve (AbsMap (mp1, def1)) = (AbsMap (mp3, def2), e2)
  where
    (mp2, e1) = Map.foldWithKey f (Map.empty, []) mp1
    (mp3, def2, e2) = if Map.null mp2
                      then case def1 of
                             Nothing  -> (mp2, def1, e1)
                             (Just (Val chn occ)) -> let (chn', e') = resolveChn chn
                                                         eOcc       = resolveOcc occ
                                                     in (mp2, Just (Val chn' (MultDef [])), subinfos eOcc e' ++ e1)
                      else case def1 of
                             Nothing  -> (mp2, def1, e1)
                             (Just v) -> let vs = v : Map.elems mp1
                                             es = concatMap valToEs vs
                                         in (mp2, Nothing, Dup (nub es) : e1)
    valToEs (Val _ occ) = occToEs occ
    occToEs (SingleDef es) = es
    occToEs (MultDef es)   = es
    occToEs (OccRequire e) = [e]
    f k (Val chn occ) (mp,e1) = let (chn', e') = resolveChn chn
                                    eOcc       = resolveOcc occ
                                in (Map.insert k (Val chn' (MultDef [])) mp, subinfos eOcc e' ++ e1)
    subinfos [] e = e
    subinfos e _  = e -- exclude submerge information

resolveOcc :: Eq e => Occurrence e -> [MergeInfo e]
resolveOcc (SingleDef es) | length es > 1 = [Dup (nub es)]
resolveOcc (OccRequire e)                 = [Missing e]
resolveOcc _                              = []

resolveChn :: (Eq e, Ord k) => Chain e k a -> (Chain e k a, [MergeInfo e])
resolveChn (ChnItem ov r) = let (ov', e1) = resolveOv ov
                                (r', e2)  = resolveChn r
                            in (ChnItem ov' r', e1 ++ e2)
resolveChn (ChnSub mp r)  = let (mp', e1) = resolve mp
                                (r', e2)  = resolveChn r
                            in (ChnSub mp' r', e1 ++ e2)
resolveChn ChnEnd         = (ChnEnd, [])

resolveOv :: Eq e => Overridable e a -> (Overridable e a, [MergeInfo e])
resolveOv (Fixed a es) = (Fixed a [head es], if length es > 1 then [Dup (nub es)] else [])
resolveOv (Absent e)   = (Absent e, [Missing e])
resolveOv o = (o, [])


--
-- Access the AbsMap
--

assocs :: AbsMap e k a -> k -> [(k, Chain e k a)]
assocs (AbsMap (mp,mbDef)) defK
  = mbItems ++ mpItems
  where
    mbItems = case mbDef of
                Nothing        -> []
                Just (Val c _) -> [(defK, c)]
    mpItems = map (\(k,(Val c _)) -> (k, c)) $ Map.assocs mp

elems :: AbsMap e k a -> [Chain e k a]
elems (AbsMap (mp,mbDef))
  = mbItems ++ mpItems
  where
    mbItems = case mbDef of
                Nothing        -> []
                Just (Val c _) -> [c]
    mpItems = map (\(Val c _) -> c) $ Map.elems mp

extract :: Overridable e a -> a -> a
extract (Replaceable x) _ = x
extract (Fixed x _) _     = x
extract (Absent _) x      = x
extract (Appendable x) _  = x
