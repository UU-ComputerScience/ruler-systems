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
            <*> pInputs
            <*> pOutputs

pCode :: AgParser Code
pCode = Code_Code <$> pOCurlyPos <*> pTks <* pCCurlyPos

pTks :: AgParser Tks
pTks = pList_gr pTk

pTk :: AgParser Tk
pTk  =   uncurry (flip Tk_String) <$> pStringPos
     <|> mkTkIdent <$> pVaridPos
     <|> Tk_Sem    <$> pOCurlyPos <*> pSem <*> pCCurlyPos
  where mkTkIdent (s,p)
          = let (before,incl) = break (== '.') (tail s)
                after = tail incl
            in Tk_Ident p (Ident before p) (Ident after p)

pSem :: AgParser Sem
pSem   =  Sem_Sem
      <$> pKeyPos "sem"
      <*  pKey "::"
      <*> pItfIdent
      <*> pClauses

pInputs :: AgParser [Ident]
pInputs = pKey "inputs" *> pList_gr pVarIdent

pOutputs :: AgParser [Ident]
pOutputs = pKey "outputs" *> pList_gr pVarIdent

pClauses :: AgParser Clauses
pClauses = pList_gr pClause

pClause :: AgParser Clause
pClause  =   Clause_Clause
        <$> pKeyPos "clause"
        <*> pStmts

pStmts :: AgParser Stmts
pStmts = pList_gr pStmt

pStmt :: AgParser Stmt
pStmt  =  Stmt_Child
           <$> pKeyPos "child"
           <*> pVarIdent
           <*  pKey "::"
           <*> pItfIdent
           <*  pKey "="
           <*> pCode
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

