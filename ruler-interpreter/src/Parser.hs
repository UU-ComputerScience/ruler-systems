{-# OPTIONS_GHC -fglasgow-exts #-}
module Parser(Parser.parse,Select(..)) where

import UU.Scanner
import UU.Scanner.GenToken
import UU.Parsing
import RulerExpr hiding (Message(..))
import LambdaExpr
import RulesExpr
import SyntaxExt
import Data.List(partition,sortBy)
import Data.Map(Map)
import qualified Data.Map as Map
import Externals -- get some instances

type RulerParser a = Parser Token a
data LamCall e = LamCall Ident [(Int,e)]

pKeyDerivation :: RulerParser Pos
pKeyDerivation = pKeyPos "derivation"

pKeyExternal :: RulerParser Pos
pKeyExternal = pKeyPos "external"

pKeyMerge :: RulerParser Pos
pKeyMerge = pKeyPos "merge"

pKeyInputs :: RulerParser Pos
pKeyInputs = pKeyPos "inputs"

pKeyVisit :: RulerParser Pos
pKeyVisit = pKeyPos "visit"

pKeyOrder :: RulerParser Pos
pKeyOrder = pKeyPos "order"

pKeyRequires :: RulerParser Pos
pKeyRequires = pKeyPos "requires"

pKeyExposes :: RulerParser Pos
pKeyExposes = pKeyPos "exposes"

pKeyOutputs :: RulerParser Pos
pKeyOutputs = pKeyPos "outputs"

pKeyHide :: RulerParser Pos
pKeyHide = pKeyPos "hide"

pKeySkip :: RulerParser Pos
pKeySkip = pKeyPos "skip"

pKeyAbstract :: RulerParser Pos
pKeyAbstract = pKeyPos "abstract"

pKeyBranch :: RulerParser Pos
pKeyBranch = pKeyPos "branch"

pKeyEstablish :: RulerParser Pos
pKeyEstablish = pKeyPos "establish"

pKeyFresh :: RulerParser Pos
pKeyFresh = pKeyPos "fresh"

pKeyEquiv :: RulerParser Pos
pKeyEquiv = pKeyPos "=="

pKeyBind :: RulerParser Pos
pKeyBind = pKeyPos ":="

pKeyInst :: RulerParser Pos
pKeyInst = pKeyPos "inst"

pKeyAs :: RulerParser Pos
pKeyAs = pKeyPos "as"

pKeyEval :: RulerParser Pos
pKeyEval = pKeyPos "eval"

pKeyLam :: RulerParser Pos
pKeyLam = pKeyPos "\\"

pKeyBar :: RulerParser Pos
pKeyBar = pKeyPos "|"

pKeyLet :: RulerParser Pos
pKeyLet = pKeyPos "let"

pKeyIn :: RulerParser Pos
pKeyIn = pKeyPos "in"

pKeyIf :: RulerParser Pos
pKeyIf = pKeyPos "if"

pKeyThen :: RulerParser Pos
pKeyThen = pKeyPos "then"

pKeyElse :: RulerParser Pos
pKeyElse = pKeyPos "else"

pKeyOf :: RulerParser Pos
pKeyOf = pKeyPos "of"

pKeyCase :: RulerParser Pos
pKeyCase = pKeyPos "case"

pKeyArrow :: RulerParser Pos
pKeyArrow = pKeyPos "->"

pKeyEqual :: RulerParser Pos
pKeyEqual = pKeyPos "="

pKeyAt :: RulerParser Pos
pKeyAt = pKeyPos "@"

pKeyInvoke :: RulerParser Pos
pKeyInvoke = pKeyPos "invoke"

pKeyResult :: RulerParser Pos
pKeyResult = pKeyPos "result"

pKeyUnderscore :: RulerParser Pos
pKeyUnderscore = pKeyPos "_"

pKeySyntax :: RulerParser Pos
pKeySyntax = pKeyPos "syntax"

pKeyKeywords :: RulerParser Pos
pKeyKeywords = pKeyPos "keywords"

pKeyDot :: RulerParser Pos
pKeyDot = pKeyPos "."

pKeyColon :: RulerParser Pos
pKeyColon = pKeyPos ":"

pKeyBang :: RulerParser Pos
pKeyBang = pKeyPos "!"

pKeyLine :: RulerParser Pos
pKeyLine = pKeyPos "-----"

pKeyAugment :: RulerParser Pos
pKeyAugment = pKeyPos "augment"

pKeyInnername :: RulerParser Pos
pKeyInnername = pKeyPos "innername"

pIdent :: RulerParser Ident
pIdent = uncurry Ident <$> pVaridPos


data Select a where
  ExprParser  :: Select Expr
  StmtsParser :: Select Stmts


parse :: Select a -> [Token] -> Either [String] a
parse s tks = parseTokens (sel s)
  where
    sel :: Select a -> RulerParser a
    sel ExprParser  = pToplevelExpr
    sel StmtsParser = pStmtsSeq
  
    pStmtDeriv = pStmt True
    pStmtSeq = pStmt False
  
    pStmt :: Bool -> RulerParser Stmts
    pStmt onDeriv
      =   -- cases that start with an Expr
          pExpr <**> (   mkEquiv <$> pKeyEquiv <*> pExpr <* pSemi
                     <|> mkBind <$> pKeyBind <*> pExpr <* pSemi )
      <|> -- cases that start with an Establish
          pKeyEstablish <**> (pIdent <**>
            (   mkEstablish <$> pLevelOverride <* pSemi
            <|> mkEqEstabl <$> pEqSyns <* pSemi ))
      -- other cases
      <|> mkInst      <$> pKeyInst <*> pExpr <* pKeyAs <*> pIdent <* pSemi
      <|> mkFresh     <$> pList1Sep_ng pComma pIdent <* pKeyFresh <* pSemi      -- watch out for ambiguities with this one
      <|> mkEval      <$> pKeyEval <*> pExpr <* pSemi
      <|> mkNop       <$> (pSyntax <|> pKeywords) <* pSemi
      <|> mkEqAugment <$> pKeyAugment <*> pIdent <*> pEqSyns <* pSemi
      <|> mkEqConcl   <$> pKeyLine <*> pList1_ng (pKeyEstablish *> pEqSynsR) <* pSemi
      <|> mkLet       <$> pKeyLet <*> pPat <* pKeyEqual <*> pExpr <* pSemi
      where
        mkInst p e nm          = wrap (Stmt_Inst p e nm)
        mkEstablish mbLvl nm p = wrap (Stmt_Establish p nm mbLvl)
        mkEquiv p r l          = wrap (Stmt_Equiv p l r)
        mkBind p r l           = wrap (Stmt_Bind p l r)
        mkFresh nms            = wrap (Stmt_Fresh (identPos (head nms)) nms)
        mkEval p e             = wrap (Stmt_Eval p e)
        mkNop p                = []
        mkEqAugment p nm o     = sem_EqStmt_Augment p nm o
        mkEqEstabl o nm p      = sem_EqStmt_Establish p nm o
        mkEqConcl p os         = sem_EqStmt_Conclusion p (foldr sem_Augmentss_Cons sem_Augmentss_Nil os)
        mkLet                  = mkStmtLet onDeriv
        wrap                   = return

    pStmtsDeriv :: RulerParser Stmts
    pStmtsDeriv = concat <$> pList1_ng pStmtDeriv

    pStmtsSeq :: RulerParser Stmts
    pStmtsSeq = concat <$> pList1_ng pStmtSeq

    pExpr :: RulerParser Expr
    pExpr = pExpr' False True
    
    pOutputExpr :: RulerParser Expr
    pOutputExpr = pExpr' True False

    pExprWithoutLambda :: RulerParser Expr
    pExprWithoutLambda = pExpr' False False

    pOutputExprWithoutLambda :: RulerParser Expr
    pOutputExprWithoutLambda = pExpr' True False

    pExpr' :: Bool -> Bool -> RulerParser Expr
    pExpr' isOutput includeLambda
      =   ( if isOutput
            then pIdent <**> (   pSucceed (Expr_Var Mode_Def)
                        <|> (\nm fld -> Expr_Field fld nm) <$ pKeyDot <*> pIdent
                        )
                 <|> pParens exprWith
            else pIdent <**> (   pSucceed (Expr_Var Mode_Ref)
                             <|> (\nm fld -> Expr_Field fld nm) <$ pKeyDot <*> pIdent
                             )
                 <|> Expr_Var <$> (Mode_Def <$ pKeyBang) <*> pIdent
                 <|> Expr_Derivation <$> pKeyDerivation <*> pOrder <*> pParamsIO <*> opt (pKeyInnername *> pIdent) (ident "this") <*> pLevel <* pOCurly <*> pAlts <* pCCurly
                 <|> Expr_External <$> pKeyExternal <*> pIdent <*> pParamsIOExtern <*> pLevel
                 <|> Expr_Merge <$> pKeyMerge <*> pCurly (pList1Sep_ng pComma exprWith)
                 <|> pParens pToplevelExpr
          )
      <|> uncurry Expr_Prim <$> pPrimVal
      <|> if includeLambda then pLambdaItf <|> pExprSynCall else pFail
      where
        exprWith     = if isOutput then pOutputExpr else pExpr
        exprWithout  = if isOutput then pOutputExprWithoutLambda else pExprWithoutLambda

    readInt :: String -> Int
    readInt = read

    readString :: String -> String
    readString = read

    pLevelOverride :: RulerParser (Maybe Level)
    pLevelOverride
      =   pSucceed Nothing
      <|> Just <$> pLevelBase

    pLevel :: RulerParser Level
    pLevel
      =   pSucceed Level_Intro
      <|> pLevelBase

    pLevelBase :: RulerParser Level
    pLevelBase
      =   Level_Hide <$ pKeyHide
      <|> Level_Skip <$ pKeySkip
      <|> Level_Abstract <$ pKeyAbstract <*> pIdent

    pPrimVal :: RulerParser (Pos, PrimVal)
    pPrimVal
      =   (\(s,p) -> (p, PrimVal $ PI $ readInt s)) <$> pIntegerPos
      <|> (\(s,p) -> (p, PrimVal $ PS $ readString s)) <$> pStringPos

    pToplevelExpr :: RulerParser Expr
    pToplevelExpr
      =   Expr_Seq <$> pStmtsSeq <*> pExpr
      <|> pExpr
    
    pOrder :: RulerParser Order
    pOrder =   Order_Relative <$ pKeyOrder <*> pList1Sep_ng pComma pIdent
           <|> pSucceed (Order_Relative [])

    pParamIO :: RulerParser Param
    pParamIO
      =   flip Param_Input  <$ pKeyInputs  <*> pList1Sep_ng pComma pIdent <*> opt (pKeyVisit *> pIdent) visitIdentMain
      <|> flip Param_Output <$ pKeyOutputs <*> pList1Sep_ng pComma pIdent <*> opt (pKeyVisit *> pIdent) visitIdentMain

    pParamIOExtern :: RulerParser Param
    pParamIOExtern
      =   Param_Input visitIdentMain  <$ pKeyInputs  <*> pList1Sep_ng pComma pIdent 
      <|> Param_Output visitIdentMain <$ pKeyOutputs <*> pList1Sep_ng pComma pIdent

    pParamsIO :: RulerParser Params
    pParamsIO = pList_ng pParamIO

    pParamsIOExtern :: RulerParser Params
    pParamsIOExtern = pList_ng pParamIOExtern

    pParamRE :: RulerParser Scope
    pParamRE
      =   Scope_Requires <$ pKeyRequires <*> pList1Sep_ng pComma pIdent
      <|> Scope_Exposes  <$ pKeyExposes  <*> pList1Sep_ng pComma pIdent

    pParamsRE :: RulerParser Scopes
    pParamsRE = pList_ng pParamRE

    pAlt :: RulerParser Alt
    pAlt
      = Alt_Alt <$ pKeyBranch <*> pIdent <*> pParamsRE <* pKeyColon <*> pStmtsDeriv

    pAlts :: RulerParser Alts
    pAlts = mergeAlts <$> pList1_ng pAlt
      where
        mergeAlts = Map.elems . Map.unionsWith merge . map (\a@(Alt_Alt nm _ _) -> Map.singleton nm a)
        merge (Alt_Alt nm p1 s1) (Alt_Alt _ p2 s2) = Alt_Alt nm (p1 ++ p2) (s1 ++ s2)


    --
    -- Lambda-calculus embedding
    --

    pLambdaItf :: RulerParser Expr
    pLambdaItf = sem_LamAGItf_AGItf <$> pLambda

    declsAlg :: (T_Decl -> T_Decls -> T_Decls, T_Decls)
    declsAlg = (sem_Decls_Cons, sem_Decls_Nil)

    casesAlg :: (T_Case -> T_Cases -> T_Cases, T_Cases)
    casesAlg = (sem_Cases_Cons, sem_Cases_Nil)

    pLambda :: RulerParser T_Lambda
    pLambda = pLambda' False

    pOutputLambda :: RulerParser T_Lambda
    pOutputLambda = pLambda' True

    pLambda' :: Bool -> RulerParser T_Lambda
    pLambda' isOutput
      =   (\l ls -> foldl1 sem_Lambda_App (l:ls)) <$> exprWithout <*> pList1_ng exprWithout
      <|> sem_Lambda_Abstract <$> pKeyAbstract <*> pIdent <* pKeyInputs <*> pList1Sep_ng pComma pIdent <*> exprWithout
      <|> sem_Lambda_Hide <$> pKeyHide <*> exprWithout
      <|> if isOutput
          then pFail
          else sem_Lambda_Lam <$> pKeyLam <*> (foldr sem_Pats_Cons sem_Pats_Nil <$> pList1_ng (sem_PatTop_Top <$> pPat)) <* pKeyArrow <*> exprWith
               <|> sem_Lambda_Let <$> pKeyLet  <*> pCurly (pFoldr1_ng declsAlg (pDecl <* pSemi))  <* pKeyIn <*> exprWith
               <|> sem_Lambda_Case <$> pKeyCase <*> exprWith <* pKeyOf <*> pCurly (pFoldr1_ng casesAlg pCase)
               <|> mkIte <$> pKeyIf <*> exprWith <* pKeyThen <*> exprWith <* pKeyElse <*> exprWith
      where
        exprWith    = if isOutput then pOutputExpr else pExpr
        exprWithout = if isOutput then pOutputExprWithoutLambda else pExprWithoutLambda
        mkIte p g t e = sem_Lambda_Case p g (sem_Cases_Cons (pat p (Ident "bTrue" p) t) $ sem_Cases_Cons (pat p (Ident "bFalse" p) e) sem_Cases_Nil)
        pat p i e = sem_Case_Case (sem_PatTop_Top $ sem_Pat_Escape p (Expr_Var Mode_Ref i)) (sem_Body_Unguarded p e)

    pDecl :: RulerParser T_Decl
    pDecl = sem_Decl_Decl <$> pPatTop <* pKeyEqual <*> pExpr

    pCase :: RulerParser T_Case
    pCase = sem_Case_Case <$> pPatTop <*> pBodies

    pBodies :: RulerParser T_Bodies
    pBodies
      =   (\p e -> sem_Bodies_Cons (sem_Body_Unguarded p e) (sem_Bodies_Nil))  <$> pKeyArrow <*> pExpr <* pSemi
      <|> foldr sem_Bodies_Cons sem_Bodies_Nil <$> pList1_ng pGuard

    pGuard :: RulerParser T_Body
    pGuard = sem_Body_Guarded <$> pKeyBar <*> pExpr <* pKeyArrow <*> pExpr <* pSemi

    pPatTop :: RulerParser T_PatTop
    pPatTop = sem_PatTop_Top <$> pPatApp

    pPatApp :: RulerParser T_Pat
    pPatApp = (\l ls -> foldl sem_Pat_App l ls) <$> pPat <*> pList_ng pPat

    pPat :: RulerParser T_Pat
    pPat =   sem_Pat_Var <$> pIdent
         <|> sem_Pat_Underscore <$> pKeyUnderscore
         <|> uncurry sem_Pat_Prim <$> pPrimVal
         <|> pParens pPatApp
         <|> sem_Pat_Escape <$> pKeyAt <*> pCurly pToplevelExpr
         <|> (\pos -> sem_Pat_Escape pos . Expr_Var Mode_Ref) <$> pKeyAt <*> pIdent


    --
    -- Syntax extensions
    --

    (tks', SynExtensions exts keys) = preprocess tks
    (lamSyns, eqSyns) = partition isLamSyn exts
    isLamSyn (LamExt _ _) = True
    isLamSyn _            = False

    pExtraKeys = foldr (\k r -> pKeyPos k <|> r) pFail keys
    pLamSyns p1 p2 = foldr (\(LamExt ident syms) r -> (LamCall ident <$> foldr (pSymLam p1 p2) (pSucceed []) syms) <|> r) pFail lamSyns
    pEqSynsDecl = foldr (\(EqExt pos syms) r -> (foldr (pSymEq pos) (pSucceed ()) syms) <|> r) pFail eqSyns

    pEqSyns  = pEqSyns' id
    pEqSynsR = pEqSyns' (map reverse)
      where reverse (EqSymInput nm)       = EqSymOutput nm
            reverse (EqSymParenInput nm)  = EqSymParenOutput nm
            reverse (EqSymOutput nm)      = EqSymInput nm
            reverse (EqSymParenOutput nm) = EqSymParenInput nm
            reverse s                     = s

    pEqSyns' :: ([EqSym] -> [EqSym]) -> RulerParser T_Augments
    pEqSyns' f = foldr (\(EqExt pos syms) r -> (foldr (pSymEqExpr pos) (pSucceed sem_Augments_Nil) (f syms)) <|> r) pFail eqSyns

    pSymLam p1 p2 (LamSymParam i)      rec = (\e r -> (i,e) : r) <$> p1 <*> rec
    pSymLam p1 p2 (LamSymParenParam i) rec = (\e r -> (i,e) : r) <$> pParens p2 <*> rec
    pSymLam _  _  (LamSymKey s)        rec = pKeyPos s *> rec

    pSymEq pos (EqSymInput nm)       rec = pIdent *> rec
    pSymEq pos (EqSymParenInput nm)  rec = pParens pIdent *> rec
    pSymEq pos (EqSymOutput nm)      rec = pKeyBang *> pIdent *> rec
    pSymEq pos (EqSymParenOutput nm) rec = pKeyBang *> pParens pIdent *> rec
    pSymEq _   (EqSymKey s)          rec = pKeyPos s *> rec
    
    pSymEqExpr pos (EqSymInput nm)       rec = (\e r -> sem_Augments_Cons (sem_Augment_Input pos nm e) r)  <$> pExpr <*> rec
    pSymEqExpr pos (EqSymParenInput nm)  rec = (\e r -> sem_Augments_Cons (sem_Augment_Input pos nm e) r)  <$> pParens pToplevelExpr <*> rec
    pSymEqExpr pos (EqSymOutput nm)      rec = (\o r -> sem_Augments_Cons (sem_Augment_Output pos nm o) r) <$> pPatOutput <*> rec
    pSymEqExpr pos (EqSymParenOutput nm) rec = (\o r -> sem_Augments_Cons (sem_Augment_Output pos nm o) r) <$> pParens pPatOutput <*> rec
    pSymEqExpr _   (EqSymKey s)          rec = pKeyPos s *> rec

    pExprSynCall       = pExprSynCall' pExprWithoutLambda pToplevelExpr
    pExprSynCallOutput = pExprSynCall' pOutputExprWithoutLambda pOutputExpr
    pExprSynCall' p1 p2 = (\(LamCall n bs) -> foldl1 sem_Lambda_App (Expr_Var Mode_Ref n : [ expr | (_,expr) <- sortBy (\(a,_) (b,_) -> a `compare` b) bs ]))
                    <$> pLamSyns p1 p2

    pSyntax :: RulerParser Pos
    pSyntax = pKeySyntax <* (   () <$ pIdent <* pCurly pLamSynsSyntaxblock
                            <|> pCurly pEqSynsDecl
                            )
    pLamSynsSyntaxblock = pLamSyns pInteger10 pInteger10

    pKeywords :: RulerParser Pos
    pKeywords = pKeyKeywords <* pCurly (pList1_ng pExtraKeys)

    pPatOutput :: RulerParser T_PatOutput
    pPatOutput = (\l ls -> foldl sem_PatOutput_App l ls) <$> pPatOutputBase <*> pList_ng pPatOutputBase
               <|> sem_PatOutput_Escape noPos <$> pExprSynCallOutput

    pPatOutputBase :: RulerParser T_PatOutput
    pPatOutputBase
      =   sem_PatOutput_Var <$> pIdent
      <|> sem_PatOutput_Field <$> pIdent <* pKeyDot <*> pIdent
      <|> sem_PatOutput_Underscore <$> pKeyUnderscore
      <|> uncurry sem_PatOutput_Prim <$> pPrimVal
      <|> pParens pPatOutput
      <|> sem_PatOutput_Escape <$> pKeyAt <*> pCurly pToplevelExpr
      <|> (\pos -> sem_PatOutput_Escape pos . Expr_Var Mode_Ref) <$> pKeyAt <*> pIdent


    --
    -- Parse process
    --

    parseTokens :: RulerParser a -> Either [String] a
    parseTokens p
      = if null msgs
        then final `seq` Right v
        else Left (map format msgs)
      where
        steps = UU.Parsing.parse p tks'
        msgs  = getMsgs steps
        (Pair v final) = evalSteps steps

format :: Message Token (Maybe Token) -> String
format (Msg exp mtok _)
  = h mtok ++ ". Expecting: " ++ show exp ++ "."
  where h Nothing  = "parse error."
        h (Just tok) = let p = position tok
                       in file p ++ ":" ++ show (line p) ++ "," ++ show (column p) ++ ": parse error at "
                          ++ case tok of
                               Reserved str _    -> "symbol " ++ show str
                               ValToken _ val _  -> show val

