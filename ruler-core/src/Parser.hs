module Parser(AgParser, parseProgram) where


import UU.Scanner
import UU.Scanner.GenToken
import UU.Parsing
import Opts
import Ast
import Common
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.Map(Map)


type AgParser a = Parser Token a

pEnd :: AgParser Pos
pEnd = pKeyPos "end" <?> "end of layout"

pVarIdent :: AgParser Ident
pVarIdent = uncurry Ident <$> pVaridPos

pConIdent :: AgParser Ident
pConIdent = uncurry Ident <$> pConidPos

pIdentItf, pIdentAttr, pIdentChild, pIdentVisit, pIdentCon :: AgParser Ident
pIdentItf    = pConIdent <?> "itf ident"
pIdentAttr   = pVarIdent <?> "attr ident"
pIdentSem    = pVarIdent <?> "sem ident"
pIdentChild  = pVarIdent <?> "child ident"
pIdentVisit  = pVarIdent <?> "visit ident"
pIdentClause = pVarIdent <?> "clause ident"
pIdentCon    = pConIdent <?> "con ident"

pProgram :: AgParser Program
pProgram = Program_Program <$> pList_gr pBlock

pBlock :: AgParser Block
pBlock  =  Block_Itf <$> pItf
       <|> Block_Section <$> pCurly pCode

pCode :: AgParser Code
pCode = Code_Code <$> pList_gr pItem

pItf :: AgParser Itf
pItf = Itf_Itf
        <$> pKeyPos "itf"
        <*> pIdentItf
        <*> pItfVisits
        <*  pEnd
        <?> "itf"

pItfVisits :: AgParser ItfVisits
pItfVisits
  = pList1_gr pItfVisit <?> "visits"

pItfVisit :: AgParser ItfVisit
pItfVisit
   = ItfVisit_Visit
       <$> pKeyPos "visit"
       <*> pIdentVisit
       <*> pList_gr pAttrInhSyn
       <?> "visit"

pAttrInhSyn :: AgParser Attr
pAttrInhSyn = (pAttrInh <|> pAttrSyn) <?> "attr"

pAttrInh :: AgParser Attr
pAttrInh = Attr_Inh <$ pKey "inh" <*> pVarIdent <* pKey "::" <*> (pTextln <?> "type") <* pEnd

pAttrSyn :: AgParser Attr
pAttrSyn = Attr_Syn <$ pKey "syn" <*> pVarIdent <* pKey "::" <*> (pTextln <?> "type") <* pEnd

pItem :: AgParser Item
pItem
  =   uncurry (flip Item_Plain) <$> (pTextlnPos <?> "code")
  <|> uncurry mkAttr <$> (pVaridPos <?> "attribute occurrence")
  <|> (Item_Sem <$> pKeyPos "sem"
               <*> pIdentSem
               <* pKey ":"
               <*> pIdentItf
               <*> pSemVisit
               <* pEnd <?> "nested sem")
  <|> (Item_Detach <$> pKeyPos "detach" <*> pIdentVisit <* pKey "of" <*> pIdentChild <* pEnd <?> "detach item")
  <?> "code item"
  where
    mkAttr s p = let (i,t) = break (== '.') s
                 in Item_Attr p (Ident i p) (Ident (tail t) p)

pSemVisit :: AgParser SemVisit
pSemVisit
  = opt ( SemVisit_Visit <$> pKeyPos "visit" <*> pIdentVisit
                         <*> pList_gr pChn <*> pList_gr pStmt
                         <*> (ClausesTop_Top <$> pList_gr pClause)
                         <* pEnd
                         <?> "visit"
        ) SemVisit_End

pChn :: AgParser VisitAttr
pChn = VisitAttr_Chn <$ pKey "chn" <*> pVarIdent <* pKey "::" <*> (pTextln <?> "type") <* pEnd <?> "visit-local attr"

pClause :: AgParser Clause
pClause = Clause_Clause <$> pKeyPos "clause" <*> pVarIdent <*> pList_gr pStmt <*> pSemVisit <* pEnd <?> "clause"

pStmt :: AgParser Stmt
pStmt
  =   ( Stmt_Eval   <$> opt (Mode_Match <$ pKey "match") Mode_Assert <*> pPat <*> pBoundCode <* pEnd <?> "eval stmt" )
  <|> ( Stmt_Invoke <$> pKeyPos "invoke" <*> pIdentVisit <* pKey "of" <*> pIdentChild <*> pBoundCode <* pEnd <?> "invoke stmt" )
  <|> ( Stmt_Attach <$> pKeyPos "attach" <*> pIdentVisit <* pKey "of" <*> pIdentChild
                    <* pKey ":" <*> pIdentItf <*> pBoundCode <* pEnd <?> "attach stmt" )
  <?> "statement"

pBoundCode :: AgParser BoundCode
pBoundCode
  =   BoundCode_Code Bind_Fun      <$> pKeyPos "="  <*> pCode
  <|> BoundCode_Code Bind_Monadic  <$> pKeyPos "<-" <*> pCode

pPat :: AgParser Pat
pPat = pChainr_gr (Pat_Cons <$ pKey ":") pPatBase

pPatCon :: AgParser Pat
pPatCon =   Pat_Con <$> pIdentCon <*> pList_gr pPatBase
        <|> pPatBase
        <?> "con pattern"

pPatBase :: AgParser Pat
pPatBase
  =   Pat_Underscore <$ pKey "_"
  <|> Pat_Attr <$> pIdentChild <* pKey "." <*> pIdentAttr
  <|> Pat_Tup <$> pParens_pCommas pPat
  <|> Pat_List <$> pBracks_pCommas pPat
  <?> "simple pattern"


--
-- Run parser
--

parseProgram :: Opts -> Pos -> [Token] -> Either String Program
parseProgram opts pos tks
  = parseTokens opts pos pProgram tks

parseTokens :: Opts -> Pos -> AgParser a -> [Token] -> Either String a
parseTokens opts pos p tks
  = if null msgs
    then final `seq` Right v
    else Left $ toError opts pos $ head msgs
  where
    steps = UU.Parsing.parse p tks
    msgs  = getMsgs steps
    (Pair v final) = evalSteps steps

toError :: Opts -> Pos -> Message Token (Maybe Token) -> String
toError opts pos (Msg exp mtok _)
  = show p ++ ": " ++ m ++ ". Expecting " ++ show exp
  where
    p = case mtok of
          Nothing -> pos
          Just t  -> position t
    m = case mtok of
          Nothing -> "end of file"
          Just tok -> case tok of
                        Reserved str _         -> "symbol " ++ show str
                        ValToken TkError val _ -> "unrecognized token " ++ show val
                        ValToken tp val _      -> let descr = if verbose opts then show tp ++ " " else ""
                                                  in descr ++ show val
