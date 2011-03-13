{-# OPTIONS_GHC -fglasgow-exts -fth #-}
module GenExternals where

import Language.Haskell.TH
import Control.Monad
import RulerExpr
import Data.Char
import qualified Data.IntSet as IntSet
import Util


--
-- Generate externals based on data types
--

genDataExternals :: String -> Name -> [Name] -> Q [Dec]
genDataExternals listname genNm types
  = do results <- mapM genDataExternal types
       let decs  = concatMap fst results
           names = concatMap snd results
       return (genExtList listname genNm names : decs)

genExtList :: String -> Name -> [Name] -> Dec
genExtList name genNm names
  = ValD (VarP (mkName name)) (NormalB $ ListE $ map genEntry names) []
  where genEntry nm = foldl AppE (VarE genNm) [ strNm nm, VarE nm ]
        strNm = LitE . StringL . transf . nameBase
        transf s = let (c:s') = drop 3 s
                   in toLower c : s'

genDataExternal :: Name -> Q ([Dec], [Name])
genDataExternal tp
  = do (TyConI (DataD _ nmD tyvars cons _)) <- reify tp
       dec <- genUnifyableInst nmD tyvars cons
       (decs, names) <- fmap unzip $ mapM (genExtFun nmD) cons
       return (dec : decs, names)

genExtFun :: Name -> Con -> Q (Dec, Name)
genExtFun nmD con
  | n == 0
      = return ( ValD (VarP nmF) (NormalB $ AppE (VarE 'mkResult1) $ ConE conNm) []
               , nmF )
  | otherwise
      = do params <- mkParams "v" n
           values <- mkParams "x" n
           return ( FunD nmF
             [ Clause [ VarP n | n <- params ]
                      ( NormalB $ DoE (
                          [ if b
                            then BindS (VarP x) $ AppE (VarE 'return) (VarE v)
                            else BindS (VarP x) $ AppE (VarE 'resolve) (VarE v)
                            | (b,v,x) <- zip3 flds params values ]
                          ++ [ NoBindS $ AppE (VarE 'mkResult1) $ foldl AppE (ConE conNm) (map VarE values) ])
                      ) []
             ]
             , nmF)
  where
    (conNm, flds) = conInfo con
    n = length flds
    nmF = mkName ("ext" ++ nameBase conNm)

conInfo :: Con -> (Name, [Bool])
conInfo (NormalC nmC fields) = (nmC, map isValueField fields)
conInfo (RecC nmC fields)    = (nmC, map isValueField' fields)
conInfo (InfixC f1 nmC f2)   = (nmC, map isValueField [f1, f2])
conInfo (ForallC _ _ con)    = conInfo con

isValueField (_, tp) = isValueTp tp
isValueField' (_, _, tp) = isValueTp tp

isValueTp (ConT v) = v == ''Value
isValueTp _        = False

mkParams :: String -> Int -> Q [Name]
mkParams prefix n = mapM (\i -> newName (prefix ++ show i)) [1..n]

genUnifyableInst :: Name -> [Name] -> [Con] -> Q Dec
genUnifyableInst nmD tyvars cons
  = do unifyClauses <- mapM genUnifyClause cons
       fallthroughClauses <- fallthrough
       fgvClauses <- mapM genFgvClause cons
       expClauses <- mapM genExpClause cons
       dgvClauses <- mapM genDgvClause cons
       return $ InstanceD (map (AppT (ConT ''Unifyable) . VarT) tyvars)
                  (AppT (ConT ''Unifyable) tp)
                  [ FunD 'unify (unifyClauses ++ fallthroughClauses)
                  , FunD 'fgv fgvClauses
                  , FunD 'dgv dgvClauses
                  , FunD 'expAll expClauses]
  where
    tp = foldl AppT (ConT nmD) (map VarT tyvars)

    fallthrough = if length cons > 1
                  then do a <- newName "a"
                          b <- newName "b"
                          return [ Clause [VarP a, VarP b] ( NormalB $ AppE (VarE 'failure) (AppE (VarE 'concat)
                                   (ListE [ AppE (VarE 'show) (VarE a)
                                          , LitE $ StringL " conflicts with "
                                          , AppE (VarE 'show) (VarE b)
                                          ]))) [] ]
                  else return []

genUnifyClause :: Con -> Q Clause
genUnifyClause con
  | n == 0
      = return $ Clause [ conPat conNm [], conPat conNm [] ]
                   ( NormalB $ AppE (VarE 'ireturn) (TupE []) ) []
  | otherwise 
      = do paramsA <- mkParams "a" n
           paramsB <- mkParams "b" n
           return $ Clause [ conPat conNm paramsA, conPat conNm paramsB ]
                      ( NormalB $ DoE $ zipWith
                          (\a b -> NoBindS $ foldl AppE (VarE 'unify) [VarE a, VarE b] )
                          paramsA paramsB
                      ) []
  where
    (conNm, flds) = conInfo con
    n = length flds

conPat :: Name -> [Name] -> Pat
conPat conNm ps = ConP conNm (map VarP ps)

genFgvClause :: Con -> Q Clause
genFgvClause con
  | n == 0
      = do acc <- newName "acc"
           return $ Clause [VarP acc, conPat conNm []]
                      ( NormalB $ AppE (VarE 'ireturn) (VarE acc) ) []
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
    (conNm, flds) = conInfo con
    n = length flds

genDgvClause :: Con -> Q Clause
genDgvClause con
  | n == 0
      = return $ Clause [conPat conNm []] (NormalB $ AppE (VarE 'ireturn) (VarE 'IntSet.empty)) []
  | otherwise
      = do params  <- mkParams "x" n
           results <- mkParams "ds" n
           return $ Clause [conPat conNm params]
                      ( NormalB $ DoE (
                          zipWith (\x r -> BindS (VarP r) $ AppE (VarE 'dgv) (VarE x)) params results
                          ++ [ NoBindS $ AppE (VarE 'ireturn) $ AppE (VarE 'IntSet.unions) $ ListE (map VarE results) ] )
                      ) []
  where
    (conNm, flds) = conInfo con
    n = length flds

genExpClause :: Con -> Q Clause
genExpClause con
  | n == 0
      = return $ Clause [WildP, conPat conNm []] (NormalB $ AppE (VarE 'return) (ConE conNm)) []
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
    (conNm, flds) = conInfo con
    n = length flds


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
  = do (TyConI (DataD _ _ _ cons _)) <- reify nm
       tblBody <- mkTablify cons
       sizeBody <- mkMinLength cons
       return (InstanceD [] (AppT (ConT ''Tabular) (ConT nm)) [tblBody, sizeBody])

mkTablify :: [Con] -> Q Dec
mkTablify cons
  = do clauses <- mapM mkClause cons
       return $ FunD 'tablify clauses
  where
    mkClause (NormalC nmC tps)
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

