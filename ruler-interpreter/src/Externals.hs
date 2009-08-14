{-# OPTIONS_GHC -fglasgow-exts -fth #-}
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
import GenExternals
import Data.Typeable
import UU.Scanner
import Control.Monad.Error
import Data.List
import Unsafe.Coerce
import System.IO.Unsafe
import UU.Scanner
import UU.Parsing


--
-- Generate external functions for data types (including corresponding Unifyable instances)
--

data Ty
  = TyVar !Value
  | TyCon !Value
  | TyArrow !Value !Value
  | TyForall !Value !Value
  deriving (Show, Typeable)

mkTyVar nm = mkFullyOpaque $ TyVar nm
mkTyConst nm = mkFullyOpaque $ TyCon nm
mkTyArrow a r = mkFullyOpaque $ TyArrow a r
mkTyForall n t = mkFullyOpaque $ TyForall n t

data Exp
  = Var !Value
  | Con !Value
  | Const !Value
  | App !Value !Value
  | Lam !Value !Value
  | SigLam !Value !Value !Value
  | Bind !Value !Value !Value
  | Fix !Value
  | Case !Value !Value
  deriving (Show, Typeable)

mkIdent n p = mkFullyOpaque (Ident n p)

mkVar v = mkFullyOpaque (Var v)
mkCon v = mkFullyOpaque (Con v)
mkConst v = mkFullyOpaque (Const v)
mkApp f a = mkFullyOpaque (App f a)
mkLam x b = mkFullyOpaque (Lam x b)
mkSigLam x t b = mkFullyOpaque (SigLam x t b)
mkBind x e b = mkFullyOpaque (Bind x e b)
mkFix f = mkFullyOpaque (Fix f)
mkCase v cs = mkFullyOpaque (Case v (convertList cs))
mkCaseAlt c xs b = mkFullyOpaque (CaseAlt c (convertList xs) b)

data CaseAlt
  = CaseAlt !Value !Value !Value
  deriving (Show, Typeable)

data ParseRes
  = ParseFail !Value | ParseSuccess !Value
  deriving (Show, Typeable)

type ExpParser a = Parser Token a

pVarIdent :: ExpParser Value
pVarIdent = uncurry mkIdent <$> pVaridPos

pConIdent :: ExpParser Value
pConIdent = uncurry mkIdent <$> pConidPos

pExp :: ExpParser Value
pExp = pExpLam

pExpLam :: ExpParser Value
pExpLam =   mkLam <$ pKeyPos "\\" <*> pVarIdent <* pKeyPos "->" <*> pExpLam
        <|> mkSigLam <$ pKeyPos "\\" <* pOParen <*> pVarIdent <* pKeyPos "::" <*> pType <* pCParen <* pKeyPos "->" <*> pExpLam
        <|> mkBind <$ pKeyPos "let" <*> pVarIdent <* pKeyPos "=" <*> pExp <* pKeyPos "in" <*> pExpLam
        <|> pExpApp

pExpApp :: ExpParser Value
pExpApp = pChainl_ng (pSucceed mkApp) pExpBase

pExpBase :: ExpParser Value
pExpBase
  =   mkVar <$> pVarIdent
  <|> mkCon <$> pConIdent
  <|> (mkConst . mkFullyOpaque . readInt) <$> pInteger
  <|> mkFix <$ pKeyPos "fix" <*> pExpBase
  <|> mkCase <$ pKeyPos "case" <*> pExp <* pKeyPos "of" <*> pCurly (pList1Sep_ng pSemi pCaseAlt)
  <|> pParens pExp

pType :: ExpParser Value
pType = mkTyForall <$ pKeyPos "forall" <*> pVarIdent <* pKeyPos "." <*> pTypeApp
      <|> pTypeApp

pTypeApp :: ExpParser Value
pTypeApp = pChainr_ng (mkTyArrow <$ pKeyPos "->") pTypeBase

pTypeBase :: ExpParser Value
pTypeBase =   mkTyVar <$> pVarIdent
          <|> mkTyConst <$> pConIdent
          <|> pParens pType

readInt :: String -> Int
readInt = read

pCaseAlt :: ExpParser Value
pCaseAlt
  = mkCaseAlt <$> pConIdent <*> pList_ng pVarIdent <* pKeyPos "->" <*> pExp


parseTokens :: ExpParser a -> [Token] -> Either [String] a
parseTokens p tks
  = if null msgs
    then final `seq` Right v
    else Left (map show msgs)
  where
    steps = parse p tks
    msgs  = getMsgs steps
    (Pair v final) = evalSteps steps

parseExpFile :: PrimString -> ParseRes
parseExpFile (PS path)
  = unsafePerformIO
    ( do tks <- scanFile ["let","in","fix","case","of","forall"] ["->","::"] "=.\\()" "->:" path
         case parseTokens pExp tks of
           Left strs -> return $ ParseFail $ mkFullyOpaque $ PS $ unlines strs
           Right exp -> return $ ParseSuccess exp
    `catch` (return . ParseFail . mkFullyOpaque . PS . show)
    )

extParseExpFile :: Value -> I (Value, ())
extParseExpFile file
  = do path <- resolve file
       let outcome = parseExpFile path
       seq outcome $ case outcome of
                       ParseSuccess exp -> do v <- mapValue exprInsertIndirections exp
                                              mkResult1 (ParseSuccess v)
                       o                -> mkResult1 o
  where
    mapValue :: (Exp -> I Exp) -> Value -> I Value
    mapValue f (ValueOpaque g a gs) = case cast a of
                                        Just a1 -> do a2 <- f a1
                                                      return (ValueOpaque g a2 gs)

-- insert indirections through variables. This in order to allow the pretty printer
-- to print only the part up to a guess.
exprInsertIndirections :: Exp -> I Exp
exprInsertIndirections = chase
  where
    chase (App f a) = do f' <- ind f
                         a' <- ind a
                         return $ App f' a'
    chase (Lam x b) = do b' <- ind b
                         return $ Lam x b'
    chase (SigLam t x b) = do b' <- ind b
                              return $ SigLam t x b'
    chase (Bind x e b) = do e' <- ind e
                            b' <- ind b
                            return $ Bind x e' b'
    chase (Fix e) = do e' <- ind e
                       return $ Fix e'
    chase (Case e cs) = do e' <- ind e
                           return $ Case e' cs
    chase v = return v
    
    ind (ValueOpaque g a gs)
      = case cast a of
          Just e -> do e' <- chase e
                       let v' = ValueOpaque g e' gs
                       x <- fresh
                       unify x v'
                       return x
    ind v = return v

data Tree
  = Node Value Value
  | Leaf Value
  deriving (Show, Typeable)

data Tuple
  = Empty
  | Single !Value
  | Tuple  !Value !Value
  | Tuple3 !Value !Value !Value
  | Tuple4 !Value !Value !Value !Value
  | Tuple5 !Value !Value !Value !Value !Value
  deriving (Show, Typeable)

chooseTup :: Int -> Ident
chooseTup 0 = ident "empty"
chooseTup 1 = ident "single"
chooseTup n = ident ("tuple" ++ show n)

data List
  = Nil
  | Cons Value Value
  deriving (Show, Typeable)

-- note: the list members do not participate in fgv calls on the list
convertList :: (Typeable a, Show a, Unifyable a, Tabular a) => [a] -> Value
convertList = foldr (\l r -> mkFullyOpaque $ Cons l r) (mkFullyOpaque Nil) . map mkFullyOpaque

$(genDataExternals "dataExternals" 'mkExtData [''Tuple, ''List, ''Bool, ''Exp, ''CaseAlt, ''ParseRes, ''Ty, ''Tree ])
$(genTabularInstances [''Tuple, ''List, ''Bool, ''Exp, ''CaseAlt, ''ParseRes, ''Ty, ''Tree])


--
-- Common external routines
--

extEnvLookup :: Value -> Value -> I (Value, ())
extEnvLookup x g
  = do x' <- resolve x :: I Ident
       g' <- resolve g :: I (Map Ident Value)
       case Map.lookup x' g' of
         Just t  -> return (t, ())
         Nothing -> abort ("extLookup: no identifier " ++ show x' ++ " in environment " ++ show g')

extEnvExtend :: Value -> Value -> Value -> I (Value, ())
extEnvExtend x t g
  = do x' <- resolve x :: I Ident
       g' <- resolve g :: I (Map Ident Value)
       mkResult1 $ Map.insert x' t g'

extEnvEmpty :: I (Value, ())
extEnvEmpty
  = mkResult1 (Map.empty :: Map Ident Value)

extMessage :: Value -> I ()
extMessage msg
  = do (PS s) <- resolve msg :: I PrimString
       message s

extAbort :: Value -> I ()
extAbort msg
  = do (PS s) <- resolve msg :: I PrimString
       abort s

extShow :: Value -> I (Value, ())
extShow v
  = do v' <- expAll guessLookup v
       mkResult1 $ PS $ show v'

extFgv :: Value -> I (Value, ())
extFgv v
  = do vs <- fgv IntSet.empty v
       return (convertList (IntSet.toList vs), ())

extConcat :: Value -> Value -> I (Value, ())
extConcat v1 v2
  = do (PS s1) <- resolve v1 :: I PrimString
       (PS s2) <- resolve v2 :: I PrimString
       mkResult1 $ PS (s1 ++ s2)

extAdd :: Value -> Value -> I (Value, ())
extAdd v1 v2
  = do i1 <- resolve v1 :: I Int
       i2 <- resolve v2 :: I Int
       mkResult1 (i1 + i2)

extMin :: Value -> Value -> I (Value, ())
extMin v1 v2
  = do i1 <- resolve v1 :: I Int
       i2 <- resolve v2 :: I Int
       mkResult1 (i1 `min` i2)

extLength :: Value -> I (Value, ())
extLength v
  = expand v >>= extLength'
  where
    extLength' (ValueOpaque _ l _)
      | (tyConString $ typeRepTyCon $ typeOf l) == "[]"
          = mkResult1 $ length $ unsafeCoerce l
      | otherwise = abort ("expecting a list, but the type is: " ++ show (typeOf l))
    extLength' v = abort ("expecting an opaque value containing a list, but encountered: " ++ show v)

extHead :: Value -> I (Value, ())
extHead v
  = do xs <- resolve v :: I [Value]
       case xs of
         x : _ -> return (x, ())
         _     -> abort "head on empty list"

extMkGuess :: Value -> I (Value, ())
extMkGuess v
  = do n <- resolve v :: I Guess
       return (ValueGuess n, ())

extFromGuess :: Value -> I (Value, ())
extFromGuess v
  = do v' <- expand v
       case v' of
         ValueGuess g -> mkResult1 g
         _            -> failure "fromGuess: not a guess"

extIsGuess :: Value -> I (Value, ())
extIsGuess v
  = do v' <- expand v
       case v' of
         ValueGuess _ -> mkResult1 True
         _            -> mkResult1 False

-- throws a failure unless the two given guesses are equal
-- note: this function could be defined in terms of isGuess and fromGuess
extEqualGuess :: Value -> Value -> I ()
extEqualGuess v1 v2
  = do g <- resolve v1 :: I Guess
       v <- expand v2  :: I Value
       case v of
         ValueGuess g' | g == g'   -> return ()
                       | otherwise -> failure "equalguess: comparing to another guess"
         _                         -> failure "equalguess: comparing to no guess"

extIdent :: Value -> I (Value, ())
extIdent v
  = do (PS s) <- resolve v
       mkResult1 (ident s)


--
-- List of external functions
--

externals :: Map Ident External
externals = Map.fromList ( 
  [ mkExt True  "lookup"       extEnvLookup
  , mkExt False "extend"       extEnvExtend
  , mkExt False "emptyenv"     extEnvEmpty
  , mkExt True  "message"      extMessage
  , mkExt True  "abort"        extAbort
  , mkExt False "show"         extShow
  , mkExt True  "fgv"          extFgv
  , mkExt False "concat"       extConcat
  , mkExt False "add"          extAdd
  , mkExt True  "min"          extMin
  , mkExt False "length"       extLength
  , mkExt False "head"         extHead
  , mkExt False "mkguess"      extMkGuess
  , mkExt False "fromguess"    extFromGuess
  , mkExt True  "isguess"      extIsGuess
  , mkExt True  "equalguess"   extEqualGuess
  , mkExt False "ident"        extIdent
  , mkExt True  "parseExpFile" extParseExpFile
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
           then Expr_Var Mode_Ref (ident "empty")
           else if length outputs == 1
                then Expr_Field nmCall (head outputs)
                else foldl sem_Lambda_App (Expr_Var Mode_Ref $ chooseTup (length outputs))
                                          (map (Expr_Field nmCall) outputs)
    
    extPos = Pos 0 0 ("<external definition " ++ show nm ++ ">")

