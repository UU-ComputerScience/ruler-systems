{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module GenExternals where

import Language.Haskell.TH
import Control.Monad
import RulerExpr
import Data.Char
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map as Map
import Data.Map(Map)
import Util
import Data.Maybe
import Data.Typeable


data GenInfo
  = GenInfo Name                  -- name of the data type
            Name                  -- name of the toIndirection function (or Indirection-constructor)
            Name                  -- name of the fromIndirection function (or Indirection-constructor)
            (Maybe Name)          -- maybe override of unifyS function
            Bool                  -- generate Tabular instance (or not)

data GenResult
  = GenResult  [Dec]      -- declarations produced
               [Name]     -- names of the ext fun produced


--
-- Generate externals based on data types
--

genDataExternals :: String -> [GenInfo] -> Q [Dec]
genDataExternals listname genInfos
  = do results <- mapM genDataExternal genInfos
       let decs  = concatMap (\(GenResult d _) -> d) results
           names = concatMap (\(GenResult _ n) -> n) results
       return (genExtList listname names : decs)

genExtList :: String -> [Name] -> Dec
genExtList name names
  = ValD (VarP (mkName name)) (NormalB $ ListE $ map genEntry names) []
  where genEntry nm = foldl AppE (VarE 'mkExtData) [ strNm nm, VarE nm ]
        strNm = LitE . StringL . transf . nameBase
        transf s = let (c:s') = drop 3 s
                   in toLower c : s'

genDataExternal :: GenInfo -> Q GenResult
genDataExternal (GenInfo tp toIndFun fromIndFun mbUnifyFun genTbl)
  = do (TyConI (DataD _ nmD tyvars cons _)) <- reify tp
       dec <- genRulerValueInst nmD tyvars cons toIndFun fromIndFun mbUnifyFun
       (decs, names) <- fmap (unzip . catMaybes) $ mapM (genExtFun nmD) cons
       let decs2 = dec : decs
       decs3 <- if genTbl
                then do decTbl <- genTabularInstance tp
                        return (decTbl : decs2)
                else return decs2
       decs4 <- if length tyvars > 0
                then do ds <- genDummyTypeable nmD tyvars
                        return (ds ++ decs3)
                else return decs3
       return $ GenResult decs4 names

genExtFun :: Name -> Con -> Q (Maybe (Dec, Name))
genExtFun nmD con
  | n == 0
      = return $ Just ( ValD (VarP nmF) (NormalB $ AppE (VarE 'mk1ResultTup) $ ConE conNm) []
                      , nmF )
  | isInd
      = return Nothing
  | otherwise
      = do params <- mkParams "v" n
           return $ Just ( FunD nmF
             [ Clause ( map VarP params )
                      ( NormalB $ AppE (VarE 'mk1ResultTup) $ foldl AppE (ConE conNm) (map VarE params)
                      ) []
             ]
             , nmF)
  where
    (conNm, flds, gFields) = conInfo con
    n = length flds
    nmF = mkName ("ext" ++ nameBase conNm)
    isInd = or gFields

conInfo :: Con -> (Name, [Bool], [Bool])
conInfo (NormalC nmC fields) = (nmC, map isVarField fields, map isGuessField fields)
conInfo (RecC nmC fields)    = (nmC, map isVarField' fields, map isGuessField' fields)
conInfo (InfixC f1 nmC f2)   = (nmC, map isVarField [f1, f2], map isGuessField [f1, f2])
conInfo (ForallC _ _ con)    = conInfo con

isVarField (_, tp) = isVarTp tp
isVarField' (_, _, tp) = isVarTp tp

isGuessField (_, tp) = isGuessTp tp
isGuessField' (_, _, tp) = isGuessTp tp

isVarTp (VarT _) = True
isVarTp _        = False

isGuessTp (ConT v) = v == ''IndInfo
isGuessTp _        = False

mkParams :: String -> Int -> Q [Name]
mkParams prefix n = mapM (\i -> newName (prefix ++ show i)) [1..n]

genRulerValueInst :: Name -> [Name] -> [Con] -> Name -> Name -> Maybe Name -> Q Dec
genRulerValueInst nmD tyvars cons toIndFun fromIndFun mbUnifyFun
  = do fallthroughClauses <- fallthrough
       unifyClauses <- case mbUnifyFun of
                         Nothing -> do cl <- mapM genUnifyClause cons
                                       return (cl ++ fallthroughClauses)
                         Just nm -> return [ Clause [] ( NormalB $ VarE nm ) [] ]
       
       fgvClauses <- mapM genFgvClause cons
       expClauses <- mapM genExpClause cons
       dgvClauses <- mapM genDgvClause cons
       toIndClauses   <- genToIndClauses nmD toIndFun
       fromIndClauses <- genFromIndClauses fromIndFun
       return $ InstanceD []
                  (AppT (ConT ''RulerValue) tp)
                  [ FunD 'unifyS unifyClauses
                  , FunD 'fgvS fgvClauses
                  , FunD 'dgvS dgvClauses
                  , FunD 'expAllS expClauses
                  , FunD 'toIndirection toIndClauses
                  , FunD 'fromIndirection fromIndClauses
                  ]
  where
    tp = foldl AppT (ConT nmD) (map VarT tyvars)

    fallthrough = if length cons > 1
                  then do a <- newName "a"
                          b <- newName "b"
                          return [ Clause [VarP a, VarP b] ( NormalB $ AppE (VarE 'failure) (AppE (VarE 'concat)
                                   (ListE [ LitE $ StringL "unification error: `"
                                          , AppE (VarE 'show) (VarE a)
                                          , LitE $ StringL "' with `"
                                          , AppE (VarE 'show) (VarE b)
                                          , LitE $ StringL "'"
                                          ]))) [] ]
                  else return []


genDummyTypeable :: Name -> [Name] -> Q [Dec]
genDummyTypeable nm vars
  = do nmD <- newName (nameBase nm ++ "Dummy")
       nmC <- newName (nameBase nm ++ "DummyCon")
       return [ DataD [] nmD [] [NormalC nmC []] [''Typeable]
              , InstanceD [] (AppT (ConT ''Typeable) $ foldl AppT (ConT nm) (map VarT vars))
                  [ FunD 'typeOf [ Clause [WildP] (NormalB $ AppE (VarE 'typeOf) (ConE nmC)) [] ]
                  ]
              ]

genFromIndClauses :: Name -> Q [Clause]
genFromIndClauses nm
  = do info <- reify nm
       case info of
         DataConI _ _ _ _ -> do ind <- newName "ind"
                                mb  <- newName "mb"
                                x   <- newName "x"
                                return [ Clause [ ConP nm [ VarP ind, VarP mb] ] ( NormalB $ TupE [AppE (ConE 'Just) (VarE ind), VarE mb] ) []
                                       , Clause [ VarP x ] ( NormalB $ TupE [ ConE 'Nothing, AppE (ConE 'Just) (VarE x) ] ) []
                                       ]
         _                -> return [ Clause [] ( NormalB $ VarE nm ) [] ]

genToIndClauses :: Name -> Name -> Q [Clause]
genToIndClauses _ nm
  = do info <- reify nm
       case info of
         DataConI _ _ _ _ -> do x <- newName "x"
                                v <- newName "v"
                                return [ Clause [ VarP x, VarP v ] ( NormalB $ AppE (VarE 'ireturn) $ foldl AppE (ConE nm) [ VarE x, VarE v] ) [] ]
         _                -> return [ Clause [] ( NormalB $ AppE (VarE 'ireturn) $ VarE nm ) [] ]

genUnifyClause :: Con -> Q Clause
genUnifyClause con
  | n == 0
      = return $ Clause [ conPat conNm [], conPat conNm [] ]
                   ( NormalB $ AppE (VarE 'ireturn) (TupE []) ) []
  | isIndNode
      = let pat = ConP conNm (map (const WildP) flds)
        in return $ Clause [ pat, pat ]
                      ( NormalB $ AppE (VarE 'abort) (LitE $ StringL "cannot unify indirections")
                      ) []
  | otherwise 
      = do paramsA <- mkParams "a" n
           paramsB <- mkParams "b" n
           return $ Clause [ conPat conNm paramsA, conPat conNm paramsB ]
                      ( NormalB $ DoE $ zipWith
                          (\a b -> NoBindS $ foldl AppE (VarE 'unify) [VarE a, VarE b] )
                          paramsA paramsB
                      ) []
  where
    (conNm, flds, gFields) = conInfo con
    n = length flds
    isIndNode = or gFields

conPat :: Name -> [Name] -> Pat
conPat conNm ps = ConP conNm (map VarP ps)

genFgvClause :: Con -> Q Clause
genFgvClause con
  | n == 0
      = do acc <- newName "acc"
           return $ Clause [VarP acc, conPat conNm []]
                      ( NormalB $ AppE (VarE 'ireturn) (VarE acc) ) []
  | isIndNode
      = let pat = ConP conNm (map (const WildP) flds)
        in return $ Clause [ WildP, pat ]
                      ( NormalB $ AppE (VarE 'abort) (LitE $ StringL "not allowed to call fgvS on indirections")
                      ) []
  | otherwise
      = do params <- mkParams "x" n
           results <- mkParams "vs" n
           acc <- newName "acc"
           let accs = acc : results
           return $ Clause [VarP acc, conPat conNm params]
                      ( NormalB $ DoE $ (
                          zipWith3 (\a x r -> BindS (VarP r) $ foldl AppE (VarE 'fgv) [VarE a, VarE x]) accs params results
                          ++ [ NoBindS $ AppE (VarE 'ireturn) $ VarE $ last accs ] )
                      ) []
  where
    (conNm, flds, gFields) = conInfo con
    n = length flds
    isIndNode = or gFields

genDgvClause :: Con -> Q Clause
genDgvClause con
  | n == 0
      = return $ Clause [conPat conNm []] (NormalB $ AppE (VarE 'ireturn) (VarE 'IntSet.empty)) []
  | isIndNode
      = let pat = ConP conNm (map (const WildP) flds)
        in return $ Clause [ pat ]
                      ( NormalB $ AppE (VarE 'abort) (LitE $ StringL "not allowed to call dgvS on indirections")
                      ) []
  | otherwise
      = do params  <- mkParams "x" n
           results <- mkParams "ds" n
           return $ Clause [conPat conNm params]
                      ( NormalB $ DoE (
                          zipWith (\x r -> BindS (VarP r) $ AppE (VarE 'dgv) (VarE x)) params results
                          ++ [ NoBindS $ AppE (VarE 'ireturn) $ AppE (VarE 'IntSet.unions) $ ListE (map VarE results) ] )
                      ) []
  where
    (conNm, flds, gFields) = conInfo con
    n = length flds
    isIndNode = or gFields

genExpClause :: Con -> Q Clause
genExpClause con
  | n == 0
      = return $ Clause [WildP, conPat conNm []] (NormalB $ AppE (VarE 'return) (ConE conNm)) []
  | isIndNode
      = let pat = ConP conNm (map (const WildP) flds)
        in return $ Clause [ WildP, pat ]
                      ( NormalB $ AppE (VarE 'fail) (LitE $ StringL "not allowed to call expAllS on indirections")
                      ) []
  | otherwise
      = do params  <- mkParams "x" n
           results <- mkParams "v" n
           lkFun   <- newName "lk"
           return $ Clause [VarP lkFun, conPat conNm params]
                      ( NormalB $ DoE $ (
                          zipWith (\x r -> BindS (VarP r) $ foldl AppE (VarE 'expAll) [VarE lkFun, VarE x]) params results
                          ++ [ NoBindS $ AppE (VarE 'return) $ foldl AppE (ConE conNm) (map VarE results) ]
                          )
                      ) []
  where
    (conNm, flds, gFields) = conInfo con
    n = length flds
    isIndNode = or gFields


--
-- Generate empty Tabular instances
--

genEmptyTabularInstances :: [Name] -> Q [Dec]
genEmptyTabularInstances
  = mapM (\nm -> return (InstanceD [] (AppT (ConT ''Tabular) (ConT nm)) []))

genTabularInstances :: [Name] -> Q [Dec]
  = mapM genTabularInstance

genTabularInstance :: Name -> Q Dec
genTabularInstance nm
  = do (TyConI (DataD _ _ vars cons _)) <- reify nm
       tblBody <- mkTablify cons
       sizeBody <- mkMinLength cons
       return (InstanceD [] (AppT (ConT ''Tabular) (foldl AppT (ConT nm) (map VarT vars))) [tblBody, sizeBody])

mkTablify :: [Con] -> Q Dec
mkTablify cons
  = do clauses <- mapM mkClause cons
       return $ FunD 'tablify clauses
  where
    mkClause con@(NormalC nmC tps)
      | isIndNode
              = do indNm      <- newName "ind"
                   contNm     <- newName "cont"
                   prioNm     <- newName "prio"
                   maxSizeNm  <- newName "maxSize"
                   return $ Clause [ VarP prioNm, VarP maxSizeNm, ConP nmC [VarP indNm, VarP contNm] ]
                      ( NormalB $ foldl AppE (VarE 'tablifyInd) [VarE prioNm, VarE maxSizeNm, VarE indNm, VarE contNm]
                      ) []
      | otherwise
              = do xs         <- mapM (const (newName "x")) tps
                   prioNm     <- newName "prio"
                   maxSizeNm  <- newName "maxSize"
                   initSizeNm <- newName "initSize"
                   tblNms     <- mapM (const (newName "t")) tps
                   accumNms   <- mapM (const (newName "a")) tps
                   recSizeNms <- mapM (const (newName "s")) tps
                   let sizeNms = initSizeNm : recSizeNms
                       recStmtss = map mkRecStmts (zip (zip xs tblNms) (zip3 accumNms sizeNms (tail sizeNms)))
                   return $ Clause [ VarP prioNm, VarP maxSizeNm, ConP nmC (map VarP xs) ]
                     (NormalB $ DoE (concat recStmtss ++ [mkRes nmC tblNms accumNms prioNm]))
                     [ ValD (VarP initSizeNm) (NormalB $
                           sub (sub (VarE maxSizeNm) (int $ length $ nameBase $ nmC))
                               (mul (int $ (length tps - 1)) (VarE 'tabDefaultMinSize))
                         ) []
                     ]
      where
        (conNm, flds, gFields) = conInfo con
        n = length flds
        isIndNode = or gFields
    mkRecStmts ((xNm, tNm), (accNm, prevSNm, curSNm))
      = [ BindS (TupP [VarP tNm, VarP accNm]) (foldl AppE (VarE 'conRecTablify) [ VarE prevSNm, VarE xNm ])
        , LetS [ ValD (VarP curSNm) (NormalB (sub (VarE prevSNm) (VarE accNm))) [] ]
        ]
    mkRes nmC tblNms accumNms prioNm
      = NoBindS (foldl AppE (VarE 'conResultTablify)
          [ VarE prioNm, LitE $ StringL $ nameBase nmC, ListE (map VarE tblNms), ListE (map VarE accumNms) ])

sub :: Exp -> Exp -> Exp
sub l r = AppE (AppE (VarE '(-)) l) r

mul :: Exp -> Exp -> Exp
mul l r = AppE (AppE (VarE '(*)) l) r

int :: Int -> Exp
int = LitE . IntegerL . toInteger

mkMinLength :: [Con] -> Q Dec
mkMinLength cons
  = do clauses <- mapM mkClause cons
       return $ FunD 'minTableLength clauses
  where
    mkClause (NormalC nmC tps)
      = do xs <- mapM (const (newName "x")) tps
           nSum <- newName "sum"
           return $ Clause [ConP nmC (map VarP xs)]
             (NormalB $ foldl AppE (VarE 'tabConSize)
               [int $ length $ nameBase nmC, VarE nSum]) [mkSum nSum xs]

    mkSum n ns = ValD (VarP n) (NormalB $ ListE $ map (\n' -> AppE (VarE 'minTableLength) (VarE n')) ns) []

