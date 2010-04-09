module Parser(AgParser, parseAg) where


import UU.Scanner
import UU.Scanner.GenToken
import UU.Parsing
import Opts
import BackendAst
import CommonTypes
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
pIdentItf   = pConIdent <?> "itf ident"
pIdentAttr  = pVarIdent <?> "attr ident"
pIdentChild = pVarIdent <?> "child ident"
pIdentVisit = pVarIdent <?> "visit ident"
pIdentCon   = pConIdent <?> "con ident"

pAg :: AgParser Ag
pAg = Ag_Ag <$> pList_gr pBlock

pBlock :: AgParser Block
pBlock  =  Block_Itf <$> pItf
       <|> Block_Section <$> pCurly pCode

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
       <*> pList_gr pParamInhSyn
       <?> "visit"

pParamInhSyn :: AgParser Param
pParamInhSyn = (pParam True <|> pParam False) <?> "param"

pParam :: Bool -> AgParser Param
pParam mode = (\n c -> Param_Param n c mode) <$ pk <*> pVarIdent <* pKey "::" <*> (pTextln <?> "type") <* pEnd
  where pk = if mode then pKey "inh" else pKey "syn"

pCode :: AgParser Code
pCode = Code_Code <$> pList_gr pItem

pItem :: AgParser Item
pItem
  =   uncurry (flip Item_Plain) <$> (pTextlnPos <?> "code")
  <|> uncurry mkAttr <$> (pVaridPos <?> "attribute occurrence")
  <|> (Item_Sem <$> pKeyPos "sem"
               <* pKey "::"
               <*> pIdentItf
               <*> pList_gr pSemVisit
               <* pEnd <?> "nested sem")
  <?> "code item"
  where
    mkAttr s p = let (i,t) = break (== '.') s
                 in Item_Attr p (Ident i p) (Ident (tail t) p)

pSemVisit :: AgParser SemVisit
pSemVisit
  = SemVisit_Visit <$>
      pKeyPos "visit" <*> pIdentVisit <*> pList_gr pStmt <?> "visit"

pStmt :: AgParser Stmt
pStmt
  =   (Stmt_Child <$> pKeyPos "child" <*> pIdentChild <* pKey "::" <*> pIdentItf <* pKey "=" <*> pCode <* pEnd <?> "child stmt")
  <|> (Stmt_Invoke <$> pKeyPos "invoke" <*> pIdentChild <*> pIdentVisit <?> "invoke stmt")
  <|> (Stmt_Eval  <$> pPat <* pKey "=" <*> pCode <* pEnd <?> "eval stmt")
  <?> "statement"

pPat :: AgParser Pat
pPat =  Pat_Con <$> pIdentCon <*> pList_gr pPatBase
    <|> Pat_Tup <$> pParens_pCommas pPat
    <|> pPatBase
    <?> "pattern"

pPatBase :: AgParser Pat
pPatBase
  =   Pat_Underscore <$ pKey "_"
  <|> Pat_Attr <$> pIdentChild <* pKey "." <*> pIdentAttr
  <|> pParens pPat
  <?> "simple pattern"

--
-- Run parser
--

parseAg :: Opts -> Pos -> [Token] -> Either String Ag
parseAg opts pos tks
  = parseTokens opts pos pAg tks

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
