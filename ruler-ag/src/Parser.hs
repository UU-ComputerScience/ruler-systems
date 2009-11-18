module Parser(pAg, parseTokens) where

import UU.Scanner
import UU.Scanner.GenToken
import UU.Parsing
import CommonTypes
import BackendAst


type AgParser a = Parser Token a

pVarIdent = uncurry Ident <$> pVaridPos
pItfIdent = uncurry Ident <$> pConidPos

pAg :: AgParser Ag
pAg = Ag_Ag <$> pList_gr pBlock

pBlock :: AgParser Block
pBlock  =  Block_Interface <$> pInterface
       <|> Block_Code <$> pCode

pInterface :: AgParser Interface
pInterface =    Interface_Interface
            <$> pKeyPos "interface"
            <*> pItfIdent
            <*> pItfVisits

pItfVisits :: AgParser ItfVisits
pItfVisits
  = pList1_gr pItfVisit

pItfVisit :: AgParser ItfVisit
pItfVisit
   =   ( ItfVisit_Visit
           <$> pKeyPos "visit"
           <*> pVarIdent
           <*> pParamSets
       )
--   To be implemented in front-end
--   <|> ( ItfVisit_Tail
--           <$> pKeyPos "tail"
--           <*> pItfIdent
--           <*> opt (Just <$ pKey ":" <*> pVarIdent) Nothing
--       )

pParamSets :: AgParser Params
pParamSets = concat <$> pList_gr (pInputs <|> pOutputs)

pInputs :: AgParser [Param]
pInputs = pKey "inputs" *> pListSep_gr (pKey ",") pParamInput

pOutputs :: AgParser [Param]
pOutputs = pKey "outputs" *> pListSep_gr (pKey ",") pParamOutput

pParamInput :: AgParser Param
pParamInput = pParam True

pParamOutput :: AgParser Param
pParamOutput = pParam False

pParam :: Bool -> AgParser Param
pParam mode = (\n c -> Param_Param n c mode) <$> pVarIdent <* pKey "::" <*> pCode

pCode :: AgParser Code
pCode = Code_Code <$> pOCurlyPos <*> pTks <* pCCurlyPos

pTks :: AgParser Tks
pTks = concat <$> pList_gr pTk

pTk :: AgParser Tks
pTk  =   (\(s,p) -> [Tk_String p s]) <$> pStringPos
     <|> (\x y z -> x ++ y ++ z) <$> key "{" <*> pTks <*> key "}"
     <|> mkTk Tk_Ident '.' <$> pVaridPos
     <|> mkTk Tk_Visit ':' <$> pConidPos
     <|> (\p1 s p2 -> [Tk_Sem p1 s p2]) <$> pKeyPos "{{" <*> pSem <*> pKeyPos "}}"
  where mkTk f c (s,p)
          = let (before,incl) = break (== c) (tail s)
                after = tail incl
            in [f p (Ident before p) (Ident after p)]
        
        key s = (\p -> [Tk_String p s]) <$> pKeyPos s

pSem :: AgParser Sem
pSem   =  Sem_Sem
      <$> pKeyPos "sem"
      <*  pKey "::"
      <*> pItfIdent
      <*> opt (Just <$ pKey ":" <*> pVarIdent) Nothing
      <*> pProds

pProds :: AgParser Prods
pProds = pList_gr pProd

pProd :: AgParser Prod
pProd
   =  Prod_Prod
  <$> pKeyPos "production"
  <*> pProdVisits

pProdVisits :: AgParser ProdVisits
pProdVisits = pList1_gr pProdVisit

pProdVisit :: AgParser ProdVisit
pProdVisit
   =  flip ProdVisit_Visit
  <$> opt (True <$ pKey "static") False
  <*> pKeyPos "visit"
  <*> pVarIdent
  <*> pClauses

pClauses :: AgParser Clauses
pClauses = pList_gr pClause

pClause :: AgParser Clause
pClause  =   Clause_Internal
               <$> pKeyPos "clause"
               <*> pStmts
         <|> Clause_External
               <$> pKeyPos "external"
               <*> pCode

pStmts :: AgParser Stmts
pStmts = pList_gr pStmt

pStmt :: AgParser Stmt
pStmt  =  (\p c f -> f p c) <$> pKeyPos "child" <*> pVarIdent <*>
            (   (\n v s p c -> Stmt_ChildSem p c n v s)
                  <$ pKey "::" <*> pItfIdent <*> opt (Just <$ pKey ":" <*> pVarIdent) Nothing <* pKey "=" <*> pCode
            <|> (\n s p c -> Stmt_VisitSem p c n s)
                  <$ pKey ":"  <*> pVarIdent <* pKey "=" <*> pCode
            )
      <|> Stmt_Match
            <$> pKeyPos "match"
            <*> pCode
            <*  pKey "="
            <*> pCode
      <|> Stmt_Eval
            <$> pKeyPos "eval"
            <*> pCode
            <*  pKey "="
            <*> pCode

parseTokens :: AgParser a -> [Token] -> Either String a
parseTokens p tks
  = if null msgs
    then final `seq` Right v
    else Left (format $ head msgs)
  where
    steps = UU.Parsing.parse p tks
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
