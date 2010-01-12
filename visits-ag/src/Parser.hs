module Parser where


import UU.Scanner
import UU.Scanner.GenToken
import UU.Parsing
import Common
import Front
import Type
import Error
import qualified Data.Sequence as Seq


type VagParser a = Parser Token a

pVarIdent :: VagParser Ident
pVarIdent = uncurry Ident <$> pVaridPos

pConIdent :: VagParser Ident
pConIdent = uncurry Ident <$> pConidPos


pVag :: VagParser Vag
pVag = Vag_Ag <$> pList_gr pDecl

pDecl :: VagParser Decl
pDecl
  =   Decl_Itf <$> pKeyPos "itf" <*> pIdentSet <*> pList_gr pItfDecl <* pEnd
  <|> Decl_Set <$> pKeyPos "set" <*> pConIdent <* pKey ":" <*> pIdentSet <* pEnd
  <|> Decl_Ctx <$> pKeyPos "ctx" <*> pIdentSet <*> opt (pKey ":" *> pList1_gr pConIdent) [] <*> pList_gr pCtxDecl <* pEnd
  <|> Decl_Forall <$> pKeyPos "forall" <*> pList_gr pVarIdent <* pKey "." <*> pIdentSet
  <|> Decl_Inh <$> pKeyPos "inh" <*> pIdentSet <*> pList_gr pSig <* pEnd
  <|> Decl_Syn <$> pKeyPos "syn" <*> pIdentSet <*> opt (pKey ":" *> pList1_gr pConIdent) [] <*> pList_gr pSig <* pEnd
  <|> Decl_Chn <$> pKeyPos "chn" <*> pIdentSet <*> opt (pKey ":" *> pList1_gr pConIdent) [] <*> pList_gr pSig <* pEnd
  <|> Decl_Tail <$> pKeyPos "tail" <*> pIdentSet <*> opt (pKey ":" *> pList1_gr pConIdent) [] <* pKey "::" <*> pInternal <* pEnd
  <|> Decl_Data <$> pKeyPos "data" <*> pIdentSet <*> pList_gr pDataDecl <* pEnd
  <|> Decl_Sem <$> pKeyPos "sem" <*> pIdentSet <*> pSemDefs <* pEnd
  <|> pCodeHead <*> pBlock <* pCCurly
  <?> "toplevel declaration"

pCodeHead :: VagParser (Block -> Decl)
pCodeHead =   ((\(p,k) -> Decl_Code p k) <$> pBlockKind <* pOCurlyPos)
          <|> ((\p -> Decl_Code p BlockKind_Code) <$> pOCurlyPos)

pItfDecl :: VagParser ItfDecl
pItfDecl
  =   ItfDecl_Ctx    <$> pKeyPos "ctx"    <*> pList_gr pConIdent <*> pList_gr pCtxDecl <* pEnd
  <|> ItfDecl_Forall <$> pKeyPos "forall" <*> pList_gr pVarIdent
  <|> ItfDecl_Inh    <$> pKeyPos "inh"    <*> pList_gr pSig <* pEnd
  <|> ItfDecl_Syn    <$> pKeyPos "syn"    <*> opt (pKey ":" *> pList1_gr pConIdent) [] <*> pList_gr pSig <* pEnd
  <|> ItfDecl_Chn    <$> pKeyPos "chn"    <*> opt (pKey ":" *> pList1_gr pConIdent) [] <*> pList_gr pSig <* pEnd
  <|> ItfDecl_Tail   <$> pKeyPos "tail"   <*> opt (pKey ":" *> pList1_gr pConIdent) [] <*> pInternal     <* pEnd
  <?> "interface declaration"

pCtxDecl :: VagParser CtxDecl
pCtxDecl
  =   CtxDecl_Syn    <$> pKeyPos "syn"    <*> pList_gr pSig <* pEnd
  <|> CtxDecl_Tail   <$> pKeyPos "tail"   <*> pInternal     <* pEnd
  <?> "context declaration"

pEnd :: VagParser Pos
pEnd = pKeyPos "end" <?> "end of layout"


pSig :: VagParser Sig
pSig = Sig_Sig <$> pList1Sep_gr pComma pVarIdent <* pKey "::" <*> pType <?> "signature"


pType :: VagParser Type
pType
  =   Type_Internal <$> pConIdent <*> pList_gr pParamType
  <|> pParamType
  <?> "type"

pParamType :: VagParser Type
pParamType
  =   Type_Var      <$> pVarIdent
  <|> uncurry (flip Type_External) <$> pExternalType
  <|> pParens pType
  <?> "param type"

pScheme :: VagParser Scheme
pScheme
  = Scheme_Quant <$> opt (pKey "forall" *> pList_gr pVarIdent <* pKey ".") [] <*> pConIdent <*> pList_gr pType <?> "type scheme"

pInternal :: VagParser Internal
pInternal
  = Internal_Type <$> pConIdent <*> pList_gr pType <?> "internal type"


pExternalType :: VagParser (String,Pos)
pExternalType
  = pCurly pTextlnPos <?> "external type"


pDataDecl :: VagParser DataDecl
pDataDecl
  =   (DataDecl_Con    <$> pKeyPos "|" <*> pIdentSet <*> pList_gr pSig  <?> "constructor declaration")
  <|> (DataDecl_Forall <$> pKeyPos "forall" <*> pList_gr pVarIdent      <?> "type variable declaration")
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
  <|> uncurry mkAttr <$> pVaridPos
  <|> Code_Sem <$> pKeyPos "sem" <*> opt (Just <$> pConIdent) Nothing <* pKey "::" <*> pScheme <*> pSemDefs <* pEnd
  <?> "code item"
  where
    mkAttr s p = let (i,t) = break (== '.') s
                 in Code_Attr p (Ident i p) (Ident (tail t) p)

pSemDefs :: VagParser SemDefs
pSemDefs
  =   SemDefs_Default <$> pKeyPos ":" <*> opt (Just <$> pConIdent) Nothing <*> pList1_gr pSemDecl
  <|> SemDefs_Clauses <$> pList_gr pClauseDecl
  <?> "semantic defs"

pSemDecl :: VagParser SemDecl
pSemDecl
  =   SemDecl_Child <$> pKeyPos "child" <*> pVarIdent <* pKey "::" <*> pInternal <* pKey "=" <*> pBlock <* pEnd
  <|> SemDecl_Visit <$> pKeyPos "visit" <*> pVarIdent <* pKey ":" <*> pConIdent <*> opt (Just <$ pKey "::" <*> pConIdent) Nothing <*> opt (Just <$ pKey "=" <*> pBlock <* pEnd) Nothing
  <|> SemDecl_Bind  <$> (BindKind_Assert <$ pKey "assert") <*> pPat <* pKey "=" <*> pBlock <* pEnd
  <|> pVarIdent <**> (   (\p b c-> SemDecl_Bind BindKind_Def (Pat_Attr c p) b) <$ pKey "." <*> pAttrPat <* pKey "=" <*> pBlock <* pEnd
                     <|> (\n b a -> SemDecl_Bind BindKind_Def (Pat_ConAttr a n) b) <$ pKey "@" <*> pConIdent <* pKey "=" <*> pBlock <* pEnd
--                     <|> (\xs t x -> SemDecl_Sig (x:xs) t) <$> pList_gr (pKey "," *> pVarIdent) <* pKey "::" <*> pType
                     )
  <|> SemDecl_Bind BindKind_Def <$> pPatRoot <* pKey "=" <*> pBlock <* pEnd
  <|> SemDecl_Tail  <$> pKeyPos "tail" <*> pBlock <* pEnd
  <?> "sem decl"

pClauseDecl :: VagParser ClauseDecl
pClauseDecl
  = ClauseDecl_Clause <$> pKeyPos "clause" <*> pIdentSet <*> opt (Just <$ pKey ":" <*> pConIdent) Nothing <*> pList_gr pSemDecl <* pEnd <?> "clause decl"

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
pPatIdent =   pVarIdent <**> (   flip Pat_Attr    <$ pKey "." <*> pAttrPat
                          <|> flip Pat_ConAttr <$ pKey "@" <*> pConIdent
                          )

pPatComplex :: VagParser Pat
pPatComplex
  =   Pat_Con <$> pConIdent <*> pList_gr pPatBase
  <|> Pat_Tup <$> pParens_pCommas pPat
  <|> pPatBase
  <?> "complex pattern"

pPatBase :: VagParser Pat
pPatBase
  =   Pat_Underscore <$ pKey "_"
  <|> pParens pPat

pAttrPat :: VagParser AttrPat
pAttrPat
  =   AttrPat_Con <$> pConIdent <*> pList_gr pAttrPatBase
  <|> AttrPat_Tup <$> pParens_pCommas pAttrPat
  <|> pAttrPatBase
  <?> "attr pattern"

pAttrPatBase :: VagParser AttrPat
pAttrPatBase
  =   AttrPat_Field <$> pVarIdent
  <|> AttrPat_Underscore <$ pKey "_"

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


--
-- Run parser
--

parseTokens :: Pos -> VagParser a -> [Token] -> Either Errs a
parseTokens pos p tks
  = if null msgs
    then final `seq` Right v
    else Left (Seq.singleton $ toError pos $ head msgs)
  where
    steps = UU.Parsing.parse p tks
    msgs  = getMsgs steps
    (Pair v final) = evalSteps steps

toError :: Pos -> Message Token (Maybe Token) -> Error
toError pos (Msg exp mtok _)
  = Error_Parse p m (show exp)
  where
    p = case mtok of
          Nothing -> pos
          Just t  -> position t
    m = case mtok of
          Nothing -> Nothing
          Just tok -> case tok of
                        Reserved str _   -> Just ("symbol " ++ show str)
                        ValToken _ val _ -> Just (show val)
