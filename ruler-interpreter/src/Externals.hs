{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module Externals where

import RulerExpr
import LambdaExpr
import Util
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.IntSet(IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap
import GenExternals
import Data.Typeable
import UU.Scanner
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.List
import Unsafe.Coerce
import System.IO.Unsafe
import UU.Scanner
import UU.Parsing
import Data.Maybe
import Opts


--
-- Generate external functions for data types (including corresponding RulerValue instances)
--


--
-- Expression and type-like data types
--

data Ty
  = TyVar !Name
  | TyCon !Name
  | TyArrow !Ty !Ty
  | TyForall !(List Name) !Ty
  | TyInd !IndInfo !(Maybe Ty)
  deriving (Show, Typeable, Eq, Ord)

data Exp
  = ExpVar   !Name
  | ExpCon   !Name
  | ExpConst !PrimInt
  | ExpApp   !Exp !Exp
  | ExpLam   !Name !Exp
  | ExpTLam  !Name !Ty !Exp
  | ExpLet   !Name !Exp !Exp
  | ExpFix   !Exp
  | ExpCase  !Exp !(List CaseAlt)
  | ExpInd   !IndInfo !(Maybe Exp)
  deriving (Show, Typeable, Eq, Ord)

data CaseAlt
  = CaseAlt !Name !(List Name) !Exp
  | CaseInd !IndInfo !(Maybe CaseAlt)
  deriving (Show, Typeable, Eq, Ord)

data ParseRes a
  = ParseFail    !PrimString
  | ParseSuccess !(PolyInd a)
  | ParseInd !IndInfo !(Maybe (ParseRes a))

instance Show (ParseRes a) where
  show (ParseFail s)         = "{Failed: " ++ show s ++ "}"
  show (ParseSuccess a)      = "{Parsed: " ++ show a ++ "}"
  show (ParseInd _ (Just t)) = show t
  show (ParseInd i Nothing)  = show i

instance Eq (ParseRes a) where
  (==) = error "dont test equaltity of ParseRes'"

instance Ord (ParseRes a) where
  compare = error "dont compare ParseRes"


type ExtParser a = Parser Token a

-- common parsers
pVarIdent :: ExtParser Name
pVarIdent = uncurry (\n p -> Name (Ident n p)) <$> pVaridPos

pConIdent :: ExtParser Name
pConIdent = uncurry (\n p -> Name (Ident n p)) <$> pConidPos

pIndList :: RulerValue a => ExtParser [a] -> ExtParser (List a)
pIndList p = (foldr (\x r -> Cons (PolyKnown x) r) Nil) <$> p

pPrimInt :: ExtParser PrimInt
pPrimInt = (PI . read) <$> pInteger

-- parser for types
pTy :: ExtParser Ty
pTy = TyForall <$ pKeyPos "forall" <*> pIndList (pList_ng pVarIdent) <* pKeyPos "." <*> pTyApp
      <|> pTyApp

pTyApp :: ExtParser Ty
pTyApp = pChainr_ng (TyArrow <$ pKeyPos "->") pTyBase

pTyBase :: ExtParser Ty
pTyBase =     TyVar <$> pVarIdent
          <|> TyCon <$> pConIdent
          <|> pParens pTy

-- parser for expressions
pExp :: ExtParser Exp
pExp = pExpLam

pExpLam :: ExtParser Exp
pExpLam =   ExpLam <$ pKeyPos "\\" <*> pVarIdent <* pKeyPos "->" <*> pExpLam
        <|> ExpTLam <$ pKeyPos "\\" <* pOParen <*> pVarIdent <* pKeyPos "::" <*> pTy <* pCParen <* pKeyPos "->" <*> pExpLam
        <|> ExpLet <$ pKeyPos "let" <*> pVarIdent <* pKeyPos "=" <*> pExp <* pKeyPos "in" <*> pExpLam
        <|> pExpApp

pExpApp :: ExtParser Exp
pExpApp = pChainl_ng (pSucceed ExpApp) pExpBase

pExpBase :: ExtParser Exp
pExpBase
  =   ExpVar   <$> pVarIdent
  <|> ExpCon   <$> pConIdent
  <|> ExpConst <$> pPrimInt
  <|> ExpFix   <$ pKeyPos "fix" <*> pExpBase
  <|> ExpCase  <$ pKeyPos "case" <*> pExp <* pKeyPos "of" <*> pIndList (pCurly (pList1Sep_ng pSemi pCaseAlt))
  <|> pParens pExp

pCaseAlt :: ExtParser CaseAlt
pCaseAlt
  = CaseAlt <$> pConIdent <*> pIndList (pList_ng pVarIdent) <* pKeyPos "->" <*> pExp


parseTokens :: ExtParser a -> [Token] -> Either [String] a
parseTokens p tks
  = if null msgs
    then final `seq` Right v
    else Left (map show msgs)
  where
    steps = parse p tks
    msgs  = getMsgs steps
    (Pair v final) = evalSteps steps

parseFile :: RulerValue a => ExtParser a -> IO [Token] -> ParseRes a
parseFile p scan
  = unsafePerformIO
    ( do tks <- scan
         case parseTokens p tks of
           Left strs -> return $ ParseFail $ PS $ unlines strs
           Right exp -> return $ ParseSuccess $ PolyKnown exp
    `catch` (return . ParseFail . PS . show)
    )

parseExpFile :: String -> ParseRes Exp
parseExpFile path
  = parseFile pExp (scanFile ["let","in","fix","case","of","forall"] ["->","::"] "=.\\()" "->:" path)

extParseExpFile :: FixedInOnly PrimString -> I (SingleRes (ParseRes Exp))
extParseExpFile (FixedInOnly (PS path))
  = do let outcome = parseExpFile path
       outcome' <- case outcome of
                          ParseFail _ -> return outcome
                          ParseSuccess e -> do e' <- mapInd expInsertIndirections e
                                               return (ParseSuccess e')
       seq outcome' (extSingleResult outcome')

-- insert indirections through variables. This in order to allow the pretty printer
-- to print only the part up to a guess.
expInsertIndirections :: Exp -> I Exp
expInsertIndirections = ind
  where
    chase (ExpApp f a) = do f' <- ind f
                            a' <- ind a
                            return $ ExpApp f' a'
    chase (ExpLam x b) = do b' <- ind b
                            return $ ExpLam x b'
    chase (ExpTLam t x b) = do b' <- ind b
                               return $ ExpTLam t x b'
    chase (ExpLet x e b) = do e' <- ind e
                              b' <- ind b
                              return $ ExpLet x e' b'
    chase (ExpFix e) = do e' <- ind e
                          return $ ExpFix e'
    chase (ExpCase e cs) = do e' <- ind e
                              return $ ExpCase e' cs
    chase e = return e

    ind :: Exp -> I Exp
    ind e = do e1 <- chase e
               e2 <- extFresh
               unify e1 e2
               return e2


--
-- Arithmetic expression
--

data Arith
  = ArithVar !Name
  | ArithConst !PrimInt
  | ArithAdd !Arith !Arith
  | ArithSub !Arith !Arith
  | ArithMul !Arith !Arith
  | ArithDiv !Arith !Arith
  | ArithLet !Name !Arith !Arith
  | ArithInd !IndInfo !(Maybe Arith)
  deriving (Show, Typeable, Eq, Ord)

parseArithFile :: String -> ParseRes Arith
parseArithFile path
  = parseFile pArith (scanFile ["let","in"] [] "=.+-/*()" "" path)

extParseArithFile :: FixedInOnly PrimString -> I (SingleRes (ParseRes Arith))
extParseArithFile (FixedInOnly (PS path))
  = do let outcome = parseArithFile path
       seq outcome (extSingleResult outcome)

pArith :: ExtParser Arith
pArith = pArithLet

pArithLet =   ArithLet <$ pKeyPos "let" <*> pVarIdent <* pKeyPos "=" <*> pArith <* pKeyPos "in" <*> pArith
          <|> pArithAdd

pArithAdd :: ExtParser Arith
pArithAdd = pChainl_ng ((ArithAdd <$ pKeyPos "+") <|> (ArithSub <$ pKeyPos "-")) pArithMul

pArithMul :: ExtParser Arith
pArithMul = pChainl_ng ((ArithMul <$ pKeyPos "*") <|> (ArithDiv <$ pKeyPos "/")) pArithBase

pArithBase :: ExtParser Arith
pArithBase
  =   ArithVar <$> pVarIdent
  <|> ArithConst <$> pPrimInt
  <|> pParens pArith


--
-- Custom data types
--

data Tree a
  = Node !(Tree a) !(Tree a)
  | Leaf !(PolyInd a)
  | IndTree !IndInfo (Maybe (Tree a))
  deriving (Show, Typeable)

instance Eq (Tree a) where
  (==) = error "dont test equaltity of Trees"

instance Ord (Tree a) where
  compare = error "dont compare Trees"


--
-- General common data types
--

data Void
  = Void
  | IndVoid !IndInfo !(Maybe Void)
  deriving (Show, Typeable, Eq, Ord)

data Tuple a b
  = Tuple !(PolyInd a) !(PolyInd b)
  | IndTuple !IndInfo !(Maybe (Tuple a b))

data Triple a b c
  = Triple !(PolyInd a) !(PolyInd b) !(PolyInd c)
  | IndTriple !IndInfo !(Maybe (Triple a b c))

instance Show (Tuple a b) where
  show (Tuple a b)           = "(" ++ show a ++ ", " ++ show b ++ ")"
  show (IndTuple _ (Just t)) = show t
  show (IndTuple i Nothing)  = show i

instance Show (Triple a b c) where
  show (Triple a b c)         = "(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")"
  show (IndTriple _ (Just t)) = show t
  show (IndTriple i Nothing)  = show i

instance Eq (Tuple a b) where
  (==) = error "dont test equaltity of Tuples"

instance Ord (Tuple a b) where
  compare = error "dont compare Tuples"

instance Eq (Triple a b c) where
  (==) = error "dont test equaltity of Triples"

instance Ord (Triple a b c) where
  compare = error "dont compare Triples"

data Boolean
  = BTrue
  | BFalse
  | IndBoolean !IndInfo !(Maybe Boolean)
  deriving (Show, Typeable, Eq, Ord)

data List a
  = Nil
  | Cons !(PolyInd a) !(List a)
  | IndList !IndInfo !(Maybe (List a))

instance Show (List a) where
  show Nil = "[]"
  show (Cons ind xs) = show ind ++ ":" ++ show xs
  show (IndList _ (Just l)) = show l
  show (IndList i Nothing)  = show i

instance Eq (List a) where
  (==) = error "dont test equaltity of Lists"

instance Ord (List a) where
  compare = error "dont compare Lists"

data Dict a b
  = DictEmpty
  | DictSingle !(PolyInd a) !(PolyInd b)
  | DictUnion !(Dict a b) !(Dict a b)
  | IndDict !IndInfo !(Maybe (Dict a b))

instance Show (Dict a b) where
  show = intercalate ", " . items
    where
      items DictEmpty = []
      items (DictSingle indA indB) = [show indA ++ " -> " ++ show indB]
      items (DictUnion d1 d2)      = items d1 ++ items d2
      items (IndDict _ (Just d))   = items d
      items (IndDict i Nothing)    = ["#" ++ show i]

instance Eq (Dict a b) where
  (==) = error "dont test equaltity of Dicts"

instance Ord (Dict a b) where
  compare = error "dont compare Dicts"

instance Tabular (Dict a b) where
  tablify prio maxSize d
    = do (tbl, size) <- tab prio (maxSize - 2) d
         return (tabBesides [ tabTxt "{", tbl, tabTxt "}" ], size+2)
    where
      tab _ _ DictEmpty = return (TableEmpty, 0)
      tab prio maxSize (DictSingle indA indB)
        = do let size = maxSize - 4
             (tabA, sizeA)   <- tablify prio size indA
             (tabB, sizeB)   <- tablify prio (size - sizeA) indB
             return (tabBesides [ tabA, tabTxt " -> ", tabB ], sizeA + sizeB + 4)
      tab prio maxSize (DictUnion d1 d2)
        = do let size = maxSize - 2
             (tabA, sizeA) <- tab prio size d1
             (tabB, sizeB) <- tab prio (size - sizeA) d2
             if sizeA == 0
              then return (tabB, sizeB)
              else if sizeB == 0
                   then return (tabA, sizeA)
                   else return (tabBesides [ tabA, tabTxt ", ", tabB ], sizeA + 2 + sizeB)
      tab prio maxSize (IndDict _ (Just dict)) = tablify prio maxSize dict
      tab prio maxSize (IndDict i Nothing)     = tablify prio maxSize i

  minTableLength DictEmpty = 0
  minTableLength (DictSingle indA indB)  = 4 + minTableLength indA + minTableLength indB
  minTableLength (DictUnion d1 d2)       = 2 + minTableLength d1 + minTableLength d2
  minTableLength (IndDict _ (Just dict)) = minTableLength dict
  minTableLength (IndDict _ _)           = tabDefaultMinSize

-- assuming that there is always an indirection on a
-- also assuming that there is only one "unknown" dictionary in a union, or
-- otherwise they will be mapped to be the same
unifyDict :: Dict a b -> Dict a b -> I ()
unifyDict d1 d2
  = do (entries1, ends1) <- chase d1 d1
       (entries2, ends2) <- chase d2 d2
       let common = Map.intersectionWith (,) entries1 entries2
           only1  = Map.difference entries1 entries2
           only2  = Map.difference entries2 entries1

       -- check if we can extend the maps to be equal
       when (  (null ends1 && not (Map.null only2))
            || (null ends2 && not (Map.null only1)) ) $
        failure "dictionaries cannot be unified, because one has more elements than the other"

       -- compare maching elements
       mapM_ match (Map.elems common)

       when (not $ Map.null only2) $
         do sequence_ (zipWith unify ends1 (tail ends1))
            unify (head ends1) (balanced only2 ends2)

       when (not $ Map.null only1) $
         do sequence_ (zipWith unify ends2 (tail ends2))
            unify (head ends2) (balanced only1 ends1)
  where
    chase _ DictEmpty = return (Map.empty, [])
    chase _ (DictSingle indA indB)
      = do a <- resolveToKnown indA
           return (Map.singleton a (indA, indB), [])
    chase _ (DictUnion d1 d2)
      = do (mp1, ends1) <- chase d1 d1
           (mp2, ends2) <- chase d2 d2
           let common = Map.intersectionWith (,) mp1 mp2
           mapM_ match (Map.elems common)
           return (Map.union mp1 mp2, ends1 ++ ends2)
    chase tl (IndDict indInfo _)
      = do d <- extResolve indInfo
           case d of
             IndDict _ Nothing   -> return (Map.empty, [tl])
             IndDict _ (Just d') -> chase tl d'

    match ((ind1,a1),(ind2,a2))
      = do unify ind1 ind2
           unify a1 a2
    
    balanced m tls = let term | null tls  = DictEmpty
                              | otherwise = head tls
                         balanced' n xs
                           | n == 1 = uncurry DictSingle (head xs)
                           | n >= 2 = let n' = n `div` 2
                                      in DictUnion (balanced' n' (take n' xs)) (balanced' (n-n') (drop n' xs))
                     in if Map.null m
                        then term
                        else DictUnion (balanced' (Map.size m) (Map.elems m)) term

data Env a
  = Env !(Map Ident (PolyInd a))
  | IndEnv !IndInfo !(Maybe (Env a))

instance Show (Env a) where
  show (Env mp)            = show mp
  show (IndEnv _ (Just e)) = show e
  show (IndEnv i Nothing)  = show i

instance Eq (Env a) where
  (==) = error "dont test equaltity of Envs"

instance Ord (Env a) where
  compare = error "dont compare Envs"

data Name
  = Name !Ident
  | IndName !IndInfo !(Maybe Name)
  deriving (Typeable, Eq, Ord)

instance Show Name where
  show (Name nm) = show nm
  show (IndName _ (Just nm)) = show nm
  show (IndName i Nothing)   = show i


-- Splice in generated code from the above data types
$(let genInfos :: [GenInfo]
      genInfos
        = [ GenInfo ''PrimString 'IndPS 'IndPS Nothing False
          , GenInfo ''PrimInt 'IndPI 'IndPI Nothing False
          , GenInfo ''Boolean 'IndBoolean 'IndBoolean Nothing True
          , GenInfo ''List 'IndList 'IndList Nothing True
          , GenInfo ''Dict 'IndDict 'IndDict (Just 'unifyDict) False
          , GenInfo ''Void 'IndVoid 'IndVoid Nothing True
          , GenInfo ''Tuple 'IndTuple 'IndTuple Nothing True
          , GenInfo ''Triple 'IndTriple 'IndTriple Nothing True
          , GenInfo ''Name 'IndName 'IndName Nothing True
          , GenInfo ''Env 'IndEnv 'IndEnv Nothing True
          , GenInfo ''Tree 'IndTree 'IndTree Nothing True
          , GenInfo ''Ty 'TyInd 'TyInd Nothing True
          , GenInfo ''Exp 'ExpInd 'ExpInd Nothing True
          , GenInfo ''CaseAlt 'CaseInd 'CaseInd Nothing True
          , GenInfo ''ParseRes 'ParseInd 'ParseInd Nothing True
          , GenInfo ''Arith 'ArithInd 'ArithInd Nothing True
          ]
  in genDataExternals "dataExternals" genInfos)


--
-- Common external routines
--


-- Concerning the result tuples
chooseTup :: Int -> Ident
chooseTup 0 = ident "void"
chooseTup 1 = ident "single"
chooseTup 2 = ident "tuple"
chooseTup 3 = ident "triple"
chooseTup n = ident ("tuple" ++ show n)


-- Debugging and printing
extMessage :: FixedInOnly PrimString -> I ()
extMessage (FixedInOnly (PS s))
  = message s

extAbort :: FixedInOnly PrimString -> I ()
extAbort (FixedInOnly (PS s))
  = abort s


-- Guess operations

-- throws failure if the passed value is not a guess.
extGetGuess :: Poly -> I (SingleRes PrimInt)
extGetGuess (Poly a)
  | isJust mbG && isNothing mbV = extSingleResult $ PI g
  | otherwise                   = failure "getguess: input value is not a guess"
  where
    (mbG, mbV)  = fromIndirection a
    (IndInfo g) = fromJust mbG

-- throws a failure unless the two given guesses are equal
extEqualGuess :: Poly -> Poly -> I ()
extEqualGuess p1 p2
  = do (SingleRes g1) <- extGetGuess p1
       (SingleRes g2) <- extGetGuess p2
       if g1 == g2
        then return ()
        else failure "equalguess: the guesses are unequal"

extFgv :: PolyArg a -> I (SingleRes (List PrimInt))
extFgv (PolyArg (Poly v))
  = do vs <- fgv IntSet.empty v
       extSingleResult (convertList $ map PI $ IntSet.toList vs)


-- string operations
extShow :: Poly -> I (SingleRes PrimString)
extShow (Poly a)
  = do a' <- expAll guessLookup a
       extSingleResult $ PS $ show a'

extStrcat :: FixedInOnly PrimString -> FixedInOnly PrimString -> I (SingleRes PrimString)
extStrcat (FixedInOnly (PS s1)) (FixedInOnly (PS s2))
  = extSingleResult $ PS (s1 ++ s2)


-- integer operations
extAdd = binIntOp (+)
extSub = binIntOp (-)
extMul = binIntOp (*)
extDiv = binIntOp div
extMin = binIntOp min
extMax = binIntOp max

binIntOp :: (Int -> Int -> Int) -> FixedInOnly PrimInt -> FixedInOnly PrimInt -> I (SingleRes PrimInt)
binIntOp op (FixedInOnly (PI i1)) (FixedInOnly (PI i2))
  = extSingleResult $ PI $ op i1 i2

extLessThen         = boolBinIntOp (<)
extGreaterThen      = boolBinIntOp (>)
extLessThenEqual    = boolBinIntOp (<=)
extGreaterThenEqual = boolBinIntOp (>=)
extIntEqual         = boolBinIntOp (==)
extIntUnequal       = boolBinIntOp (/=)

boolBinIntOp :: (Int -> Int -> Bool) -> FixedInOnly PrimInt -> FixedInOnly PrimInt -> I (SingleRes Boolean)
boolBinIntOp op (FixedInOnly (PI i1)) (FixedInOnly (PI i2))
  = case op i1 i2 of
      True  -> extSingleResult BTrue
      False -> extSingleResult BFalse


-- identifier operations
extIdent :: FixedInOnly PrimString -> I (SingleRes Name)
extIdent (FixedInOnly (PS nm))
  = extSingleResult $ Name (ident nm)


-- list operations
convertList :: RulerValue a => [a] -> List a
convertList = foldr (\x -> Cons (PolyKnown x)) Nil

extConcat :: List a -> List a -> I (SingleRes (List a))
extConcat xs ys
  = do xs' <- glue xs
       extSingleResult xs'
  where
    glue l = extExpand l >>= glue'

    glue' Nil = return ys
    glue' (Cons a r) = do r' <- glue r
                          return (Cons a r')
    glue' (IndList _ (Just r)) = glue r
    glue' (IndList _ Nothing)  = abort "cannot concat a list that is itself a variable"

extLength :: List a -> I (SingleRes PrimInt)
extLength xs
  = do i <- chase xs
       extSingleResult $ PI i
  where
    chase l = extExpand l >>= chase'
    chase' Nil        = return 0
    chase' (Cons _ r) = do r' <- chase r
                           return (1+r')
    chase' (IndList _ (Just r)) = chase r
    chase' (IndList _ Nothing)  = abort "cannot determine the length of a list containing a variable"

extHead :: FixedInOnly (List (PolyArg a)) -> I (SingleRes (PolyArg a))
extHead (FixedInOnly Nil) = abort "head on an empty list"
extHead (FixedInOnly (Cons a _)) = extSingleResult (indToPoly a)


-- environment operations
extEnvLookup :: FixedInOnly Name -> FixedInOnly (Env (PolyArg a)) -> I (SingleRes (PolyArg a))
extEnvLookup (FixedInOnly (Name x)) (FixedInOnly (Env gam))
  = case Map.lookup x gam of
      Nothing -> abort ("lookup: identifier " ++ show x ++ " not in environment " ++ show gam)
      Just p  -> extSingleResult (indToPoly p)

extEnvExtend :: FixedInOnly Name -> PolyArg a -> FixedInOnly (Env a) -> I (SingleRes (Env a))
extEnvExtend (FixedInOnly (Name nm)) v (FixedInOnly (Env mp))
  = extSingleResult $ Env $ Map.insert nm (polyToInd v) mp

extEnvEmpty :: I (SingleRes (Env a))
extEnvEmpty = extSingleResult (Env Map.empty)


--
-- List of external functions
--

externals :: Map Ident External
externals = Map.fromList ( 
  [ -- debugging and error reporting
    mkExt True  "message"           extMessage
  , mkExt True  "abort"             extAbort

  -- guess operations
  , mkExt False "getguess"          extGetGuess
  , mkExt True  "equalguess"        extEqualGuess
  , mkExt True  "fgv"               extFgv

  -- string operations
  , mkExt False "show"              extShow
  , mkExt False "strcat"            extStrcat

  -- integer operations
  , mkExt True "add"                extAdd
  , mkExt True "sub"                extSub
  , mkExt True "mul"                extMul
  , mkExt True "div"                extDiv
  , mkExt True "min"                extMin
  , mkExt True "max"                extMax
  , mkExt True "lessthen"           extLessThen
  , mkExt True "greaterthen"        extGreaterThen
  , mkExt True "lessthenequal"      extLessThen
  , mkExt True "greaterthenequal"   extGreaterThen
  , mkExt True "intequal"           extIntEqual
  , mkExt True "intunequal"         extIntUnequal

  -- identifier operations
  , mkExt False "ident"             extIdent

  -- list operations
  , mkExt False "concat"            extConcat
  , mkExt False "length"            extLength
  , mkExt False "head"              extHead

  -- environment operations
  , mkExt True  "lookup"            extEnvLookup
  , mkExt False "extend"            extEnvExtend
  , mkExt False "emptyenv"          extEnvEmpty

  , mkExt True  "parseExpFile"      extParseExpFile
  , mkExt True  "parseArithFile"    extParseArithFile
  ] ++ dataExternals )

execExternal :: Ident -> Params -> I ()
execExternal nm params
  = case Map.lookup nm externals of
      Just (External f _) -> do args <- mapM lookupIdent [ i | Param_Input visitIdentMain inps <- params, i <- inps ]
                                outs <- mapM lookupIdent [ i | Param_Output visitIdentMain outs <- params, i <- outs ]
                                res <- appExternal f args
                                sequence_ $ zipWith unify outs res
      Nothing -> abort ("No such external computation named: " ++ show nm)
    `catchError` extendError False ("executing external " ++ show nm)

--
-- Convert externals to Lambdas
--

externalsToStmts :: Map Ident External -> [Stmt]
externalsToStmts exts
  = intros ++ unifs
  where
    stmts = concatMap (uncurry externalToStmts) (Map.assocs externals)
    (intros,unifs) = partition isStmtFresh stmts

externalToStmts :: Ident -> External -> [Stmt]
externalToStmts nm (External f isVisible)
  = [ Stmt_Fresh extPos [nm]
    , Stmt_Equiv extPos (Expr_Var Mode_Def nm) lam
    ]
  where
    inputs  = map (\n -> Ident ("i" ++ show n) extPos) [1 .. arguments f]
    outputs = map (\n -> Ident ("o" ++ show n) extPos) [1 .. results f]
    lvl    = if isVisible
             then Level_Intro
             else Level_Hide
    nmCall = Ident ("call_" ++ show nm) extPos
    lam  = sem_Lambda_Lam extPos (foldr sem_Pats_Cons sem_Pats_Nil $ map (sem_PatTop_Top . sem_Pat_Var) inputs) (Expr_Seq stmts expr)
    stmts =    [ Stmt_Inst extPos (Expr_External extPos nm [Param_Input visitIdentMain inputs, Param_Output visitIdentMain outputs] lvl) nmCall ]
            ++ map (\n -> Stmt_Equiv extPos (Expr_Field nmCall n) (Expr_Var Mode_Ref n)) inputs
            ++ [ Stmt_Establish extPos nmCall Nothing ]
    expr = if null outputs
           then Expr_Var Mode_Ref (ident "void")
           else if length outputs == 1
                then Expr_Field nmCall (head outputs)
                else foldl sem_Lambda_App (Expr_Var Mode_Ref $ chooseTup (length outputs))
                                          (map (Expr_Field nmCall) outputs)
    
    extPos = Pos 0 0 ("<external definition " ++ show nm ++ ">")

