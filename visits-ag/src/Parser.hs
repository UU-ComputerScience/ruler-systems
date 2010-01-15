module Parser(VagParser, parseVag) where


import UU.Scanner
import UU.Scanner.GenToken
import UU.Parsing
import Opts
import Common
import Front
import Type
import Error
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.Map(Map)


type VagParser a = Parser Token a


--
-- Different parsers for identifiers
--

pVarIdent :: VagParser Ident
pVarIdent = uncurry Ident <$> pVaridPos

pConIdent :: VagParser Ident
pConIdent = uncurry Ident <$> pConidPos

pIdentClause, pIdentData, pIdentCon, pIdentItf, pIdentCtx, pIdentSem,
 pIdentNameSet, pIdentTypeVar, pIdentVar, pIdentChild, pIdentVisit, pIdentType, pIdentField :: VagParser Ident
pIdentClause  = pConIdent <?> "clause identifier"
pIdentData    = pConIdent <?> "data identifier"
pIdentCon     = pConIdent <?> "constructor identifier"
pIdentItf     = pConIdent <?> "interface identifier"
pIdentCtx     = pConIdent <?> "context identifier"
pIdentSem     = pConIdent <?> "semantics name"
pIdentNameSet = pConIdent <?> "ident set identifier"
pIdentTypeVar = pVarIdent <?> "type variable identifier"
pIdentVar     = pVarIdent <?> "variable identifier"
pIdentChild   = pVarIdent <?> "child identifier"
pIdentVisit   = pConIdent <?> "visit identifier"
pIdentType    = pConIdent <?> "type identifier"
pIdentField   = pVarIdent <?> "field identifier"
pIdentTail    = pVarIdent <?> "tail identifier"


--
-- Sets of ConIdents
--

pIdentSet :: VagParser IdentSet
pIdentSet = pChainr_gr (IdentSet_Excl <$ pKey "-") pIdentSetSeq <?> "ident set"

pIdentSetSeq :: VagParser IdentSet
pIdentSetSeq = foldl1 IdentSet_Union <$> pList1_gr pIdentSetBase

pIdentSetBase :: VagParser IdentSet
pIdentSetBase
  =   IdentSet_Ident <$> pConIdent
  <|> pParens pIdentSet

pIdentSetCtx, pIdentSetItf, pIdentSetData, pIdentSetCon, pIdentSetSem, pIdentSetClause :: VagParser IdentSet
pIdentSetCtx    = pIdentSet <?> "context ident set"
pIdentSetItf    = pIdentSet <?> "itf ident set"
pIdentSetData   = pIdentSet <?> "data ident set"
pIdentSetCon    = pIdentSet <?> "con ident set"
pIdentSetSem    = pIdentSet <?> "sem ident set"
pIdentSetClause = pIdentSet <?> "clause ident set"


--
-- AST parsers
--

pVag :: VagParser Vag
pVag = Vag_Ag <$> pList_gr pDecl

pDecl :: VagParser Decl
pDecl
  =   Decl_Itf <$> pKeyPos "itf" <*> pIdentSetItf <*> pList_gr pItfDecl <* pEnd
  <|> Decl_Set <$> pKeyPos "set" <*> pIdentNameSet <* pKey ":" <*> pIdentSet <* pEnd
  <|> Decl_Ctx <$> pKeyPos "ctx" <*> pIdentSetItf <*> opt (pKey ":" *> pList1_gr pIdentCtx) [] <*> pList_gr pCtxDecl <* pEnd
  <|> Decl_Inh <$> pKeyPos "inh" <*> pIdentSetItf <*> pList_ng pSig <* pEnd
  <|> Decl_Syn <$> pKeyPos "syn" <*> pIdentSetItf <*> opt (pKey ":" *> pList1_gr pIdentCtx) [] <*> pList_ng pSig <* pEnd
  <|> Decl_Chn <$> pKeyPos "chn" <*> pIdentSetItf <*> opt (pKey ":" *> pList1_gr pIdentCtx) [] <*> pList_ng pSig <* pEnd
  <|> Decl_Tail <$> pKeyPos "tail" <*> pIdentSetItf <*> opt (pKey ":" *> pList1_gr pIdentCtx) [] <*> opt (Just <$> pIdentTail <* pKey "::") Nothing <*> pInternal <* pEnd
  <|> Decl_Data <$> pKeyPos "data" <*> pIdentSetData <*> pList_gr pDataDecl <* pEnd
  <|> Decl_Sem <$> pKeyPos "sem" <*> pIdentSetSem <*> pSemDefs <* pEnd
  <|> pCodeHead <*> pBlock <* pCCurly
  <?> "toplevel declaration"

pCodeHead :: VagParser (Block -> Decl)
pCodeHead =   ((\(p,k) -> Decl_Code p k) <$> pBlockKind <* pOCurlyPos)
          <|> ((\p -> Decl_Code p BlockKind_Code) <$> pOCurlyPos)

pItfDecl :: VagParser ItfDecl
pItfDecl
  =   ItfDecl_Ctx    <$> pKeyPos "ctx"    <*> pList_gr pIdentCtx <*> pList_gr pCtxDecl <* pEnd
  <|> ItfDecl_Forall <$> pKeyPos "forall" <*> pList_gr pIdentTypeVar
  <|> ItfDecl_Inh    <$> pKeyPos "inh"    <*> pList_gr pSig <* pEnd
  <|> ItfDecl_Syn    <$> pKeyPos "syn"    <*> opt (pKey ":" *> pList_ng pIdentCtx) [] <*> pList_gr pSig <* pEnd
  <|> ItfDecl_Chn    <$> pKeyPos "chn"    <*> opt (pKey ":" *> pList_ng pIdentCtx) [] <*> pList_gr pSig <* pEnd
  <|> ItfDecl_Tail   <$> pKeyPos "tail"   <*> opt (pKey ":" *> pList_ng pIdentCtx) [] <*> opt (Just <$> pIdentTail <* pKey "::") Nothing <*> pInternal <* pEnd
  <?> "interface declaration"

pCtxDecl :: VagParser CtxDecl
pCtxDecl
  =   CtxDecl_Syn    <$> pKeyPos "syn" <*> pList_ng pSig <* pEnd
  <|> CtxDecl_Tail   <$> pKeyPos "tail" <*> opt (Just <$> pIdentTail <* pKey "::") Nothing <*> pInternal     <* pEnd
  <?> "context declaration"

pEnd :: VagParser Pos
pEnd = pKeyPos "end" <?> "end of layout"


pSig :: VagParser Sig
pSig = Sig_Sig <$> pList1Sep_gr pComma pIdentField <* pKey "::" <*> pType <?> "signature"


pType :: VagParser Type
pType
  =   Type_Internal <$> pInternal
  <|> pParamTypeBase
  <?> "type"

pParamTypeWithInternal :: VagParser Type
pParamTypeWithInternal
  =    Type_Internal <$> pInternalSingle
  <|>  pParamTypeBase
  <?> "param type"

pParamTypeWithArgsInternal :: VagParser Type
pParamTypeWithArgsInternal
  =   Type_Internal <$> pInternalMany
  <|> pParamTypeBase
  <?> "param type"

pParamTypeBase :: VagParser Type
pParamTypeBase
  =   uncurry (flip Type_External) <$> pExternalType
  <|> Type_Var <$> pIdentTypeVar
  <|> (\xs -> if length xs == 1 then head xs else Type_Tup xs) <$> pParens_pCommas pType
  <|> Type_List <$> pBracks pType
  <?> "param type"

pScheme :: VagParser Scheme
pScheme
  = Scheme_Quant <$> opt (pKey "forall" *> pList_gr pIdentVar <* pKey ".") [] <*> pInternal <?> "itf scheme"

pInternal :: VagParser Internal
pInternal
  = Internal_Type <$> pIdentType <*> pTypeArgs <?> "internal type"

pInternalSingle :: VagParser Internal
pInternalSingle
  = flip Internal_Type (TypeArgs_Types []) <$> pIdentType <?> "internal type"

pInternalMany :: VagParser Internal
pInternalMany
  = Internal_Type <$> pIdentType <*> pTypeArgsWithoutSubst <?> "internal type"

pTypeArgs :: VagParser TypeArgs
pTypeArgs
  =   pTypeArgsWithoutSubst
  <|> (TypeArgs_Subst . Map.fromList <$> pList1_ng pSubst <?> "type substitution")

pTypeArgsWithoutSubst :: VagParser TypeArgs
pTypeArgsWithoutSubst
  = TypeArgs_Types <$> pList_ng pParamTypeWithInternal <?> "type argument list"


pSubst :: VagParser (Ident, Type)
pSubst = (,) <$> pIdentTypeVar <* pKey ":=" <*> pParamTypeWithArgsInternal

pExternalType :: VagParser (String,Pos)
pExternalType
  = pCurly pTextlnPos <?> "external type"


pDataDecl :: VagParser DataDecl
pDataDecl
  =   (DataDecl_Con    <$> pKeyPos "|" <*> pIdentSetCon <*> pList_ng pSig <?> "constructor declaration")
  <|> (DataDecl_Forall <$> pKeyPos "forall" <*> pList_gr pIdentTypeVar    <?> "type variable declaration")
  <?> "data declaration"


pBlock :: VagParser Block
pBlock = Block_Code <$> pList_gr pCode <?> "code block"

pBlockKind :: VagParser (Pos, BlockKind)
pBlockKind
  =   k BlockKind_Code "code"
  <|> k BlockKind_Import "import"
  <|> k BlockKind_Preamble "header"
  <?> "block kind"
  where k c s = (\p -> (p, c)) <$> pKeyPos s

pCode :: VagParser Code
pCode
  =   uncurry (flip Code_Plain) <$> pTextlnPos
  <|> uncurry mkAttr <$> (pVaridPos <?> "attribute occurrence")
  <|> Code_Sem <$> pKeyPos "sem" <*> opt (Just <$> pIdentSem) Nothing <* pKey "::" <*> pScheme <*> pSemDefs <* pEnd
  <?> "code item"
  where
    mkAttr s p = let (i,t) = break (== '.') s
                 in Code_Attr p (Ident i p) (Ident (tail t) p)

pSemDefs :: VagParser SemDefs
pSemDefs
  =   SemDefs_Default <$> pKeyPos ":" <*> opt (Just <$> pIdentCtx) Nothing <*> pList1_gr pSemDecl
  <|> SemDefs_Clauses <$> pList_gr pClauseDecl
  <?> "semantic defs"

pSemDecl :: VagParser SemDecl
pSemDecl
  =   SemDecl_Child <$> pKeyPos "child" <*> pIdentChild <* pKey "::" <*> pInternal <* pKey "=" <*> pBlock <* pEnd
  <|> SemDecl_Visit <$> pKeyPos "visit" <*> pIdentChild <*> opt (Just <$ pKey "." <*> pIdentTail) Nothing <*> opt (Just <$ pKey ":" <*> pIdentCtx) Nothing <*> opt (Just <$ pKey "=" <*> pBlock <* pEnd) Nothing
  <|> SemDecl_Bind  <$> (BindKind_Assert <$ pKey "assert") <*> pPat <* pKey "=" <*> pBlock <* pEnd
  <|> pIdentChild <**> (   ( (\p b c-> SemDecl_Bind BindKind_Def (Pat_Attr c p) b) <$ pKey "." <*> pAttrPat <* pKey "=" <*> pBlock <* pEnd <?> ".attr-pattern")
                       <|> ( (\t n b a -> SemDecl_Bind BindKind_Def (Pat_ConAttr a t n) b) <$ pKey "@" <*> pIdentData <* pKey "." <*> pIdentCon <* pKey "=" <*> pBlock <* pEnd <?> "@con-decompose pattern")
                     )
  <|> SemDecl_Bind BindKind_Def <$> pPatRoot <* pKey "=" <*> pBlock <* pEnd
  <|> SemDecl_Tail  <$> pKeyPos "tail" <*> pBlock <* pEnd
  <?> "sem decl"

pClauseDecl :: VagParser ClauseDecl
pClauseDecl
  = ClauseDecl_Clause <$> pKeyPos "clause" <*> pIdentSetClause <*> opt (Just <$ pKey ":" <*> pIdentCtx) Nothing <*> pList_gr pSemDecl <* pEnd <?> "clause decl"

pPat :: VagParser Pat
pPat =   pPatIdent
     <|> pPatComplex
     <?> "pattern"

pPatRoot :: VagParser Pat
pPatRoot
  =   pPatIdent
  <|> pPatBase
  <?> "toplevel pattern"

pPatIdent :: VagParser Pat
pPatIdent =   pVarIdent <**> (   ( flip Pat_Attr                 <$ pKey "." <*> pAttrPat  <?> ".attr-pattern")
                             <|> ( (\t n a -> Pat_ConAttr a t n) <$ pKey "@" <*> pIdentData <* pKey "." <*> pIdentCon <?> "@con-decompose pattern")
                             )

pPatComplex :: VagParser Pat
pPatComplex
  =   Pat_Con <$> pIdentCon <*> pList_gr pPatBase
  <|> Pat_Tup <$> pParens_pCommas pPat
  <|> pPatBase
  <?> "complex pattern"

pPatBase :: VagParser Pat
pPatBase
  =   Pat_Underscore <$ pKey "_"
  <|> pParens pPat
  <?> "base pattern"

pAttrPat :: VagParser AttrPat
pAttrPat
  =   AttrPat_Con <$> pIdentCon <*> pList_gr pAttrPatBase
  <|> AttrPat_Tup <$> pParens_pCommas pAttrPat
  <|> pAttrPatBase
  <?> "attr pattern"

pAttrPatBase :: VagParser AttrPat
pAttrPatBase
  =   AttrPat_Field <$> pIdentField
  <|> AttrPat_Underscore <$ pKey "_"


--
-- Run parser
--

parseVag :: Opts -> Pos -> [Token] -> Either Errs Vag
parseVag opts pos tks
  = parseTokens opts pos pVag tks

parseTokens :: Opts -> Pos -> VagParser a -> [Token] -> Either Errs a
parseTokens opts pos p tks
  = if null msgs
    then final `seq` Right v
    else Left (Seq.singleton $ toError opts pos $ head msgs)
  where
    steps = UU.Parsing.parse p tks
    msgs  = getMsgs steps
    (Pair v final) = evalSteps steps

toError :: Opts -> Pos -> Message Token (Maybe Token) -> Error
toError opts pos (Msg exp mtok _)
  = Error_Parse p m (show exp)
  where
    p = case mtok of
          Nothing -> pos
          Just t  -> position t
    m = case mtok of
          Nothing -> Nothing
          Just tok -> case tok of
                        Reserved str _         -> Just ("symbol " ++ show str)
                        ValToken TkError val _ -> Just ("unrecognized token " ++ show val)
                        ValToken tp val _      -> let descr = if optVerbose opts then show tp ++ " " else ""
                                                  in Just (descr ++ show val)
