-- | N.B. The abstract syntax is somewhat more general than allowed. The
--        parser ensures that the AST satisfies the invariants not enforced
--        by the data definition.
module Base.Parser where

import System.IO
import Data.Set(Set)
import qualified Data.Set as Set

import Util.Parsing.Parser
import Util.Parsing.Support
import Util.Parsing.Token
import Util.Format.Format
import Util.Pattern.Pattern
import Util.Common

import Base.Base
import Base.Scanner


-- |Parse a file in base syntax.
parseModule :: String -> IO (Either Errors Module)
parseModule filename
  = do contents <- readFile filename
       return $ parseContents filename contents

-- |Parse a string of text in case syntax. Either returns errors or a module.
parseContents :: String -> String -> Either Errors Module
parseContents filename str
  = let (m, errs) = run pos pModule nextToken unsafe str
    in if null errs
       then Right m
       else Left errs
  where
    unsafe = Set.fromList [SymIdent, SymReserved "#", SymReserved "-", SymReserved "^"]
    pos    = Pos 1 1 filename


pModule :: TokenParser Module
pModule
  = Module_Mod
  <$  pKeyModule
  <*> pIdentifier
  <*> pList pDecl

pIdentifier :: TokenParser Identifier
pIdentifier
  = uncurry (flip Ident) <$> pIdent

pDecl :: TokenParser Decl
pDecl
  =   Decl_Type <$ pKeyType <*> pIdentifier <*> pList pTypeDecl <*> pReturn False
  <|> Decl_Relation <$ pKeyRelation <*> pIdentifier <*> pList pRelDecl <*> pReturn False
  <|> pKeyExternal **> (   Decl_Type <$ pKeyType <*> pIdentifier <*> pList pExternalTypeDecl <*> pReturn True
                       <|> Decl_Relation <$ pKeyRelation <*> pIdentifier <*> pList pRelDecl <*> pReturn True)

pExternalTypeDecl :: TokenParser TypeDecl
pExternalTypeDecl
  = TypeDecl_Format <$> pKeyFormat <*> pFormat True

pTypeDecl :: TokenParser TypeDecl
pTypeDecl
  =   TypeDecl_Alt    <$ pKeyAlt <*> pIdentifier <*> pList pAltDecl
  <|> TypeDecl_Format <$ pKeyGeneral <*> pKeyFormat <*> pFormat True

pAltDecl :: TokenParser AltDecl
pAltDecl
  =   AltDecl_Pattern <$> pKeyPattern <*> pPatternInfo
  <|> AltDecl_Format  <$> pKeyFormat  <*> pFormat True
  <|> AltDecl_Hole    <$ pKeyHole    <*> pIdentifier <** pKeyDColon <*> pIdentifier

pRelDecl :: TokenParser RelDecl
pRelDecl
  =   RelDecl_Rule    <$ pKeyRule <*> pIdentifier <*> pList pRuleDecl
  <|> RelDecl_Hole    <$ pKeyHole <*> pIdentifier <** pKeyDColon <*> pIdentifier
  <|> RelDecl_Pattern <$> pKeyPattern <*> pPatternInfo
  <|> RelDecl_Format  <$> pKeyFormat <*> pFormat True

pRuleDecl :: TokenParser RuleDecl
pRuleDecl
  =   RuleDecl_Ident <$ pKeyIdent <*> pIdentifier <** pKeyDColon <*> pIdentifier <** pKeyFormat <*> pFormat False
  <|> (\n t (p,i) -> RuleDecl_Judge (Kind_Premise t) n p i) <$ pKeyPrem <*> pIdentifier <** pKeyDColon <*> pIdentifier <*> pRawInstance
  <|> (\n (p,i)   -> RuleDecl_Judge Kind_Conclusion n p i) <$ pKeyConc <*> pIdentifier <*> pRawInstance

pPatternInfo :: TokenParser PatternInfo
pPatternInfo
  = (PatternInfo_Info <$> pFixity <*> (snd <$> pInt)) `opt` PatternInfo_PatOnly <*> pPattern

pFixity :: TokenParser Fixity
pFixity
  =   Infix_Left  <$ pKeyInfixl
  <|> Infix_Right <$ pKeyInfixr
  <|> Infix       <$ pKeyInfix

pPattern :: TokenParser Pattern
pPattern
  = pPatternBase <??> (flip Pattern_Seq <$> pPattern)

pPatternBase :: TokenParser Pattern
pPatternBase
  =   Pattern_Identifier                       <$> pIdentifier
  <|> (Pattern_Keyword . uncurry (flip Ident)) <$> pString
  <|> withIt kw Pattern_Seq "(" ")" <$> pPacked pKeyOParen pKeyCParen pPattern
  <|> withIt kw Pattern_Seq "{" "}" <$> pPacked pKeyOBrack pKeyCBrack pPattern
  <|> withIt kw Pattern_Seq "[" "]" <$> pPacked pKeyOSquar pKeyCSquar pPattern
  where kw = Pattern_Keyword . identFromString

pFormat :: Bool -> TokenParser Format
pFormat includeIdents
  = pChainl (Format_Above <$ pKeyAbove) pLvl
  where pLvl = pFormatLevel includeIdents

pFormatLevel :: Bool -> TokenParser Format
pFormatLevel includeIdents
  = pRec
  where pRec  = pTail <??> (flip Format_Seq <$> pRec)
        pTail = pFormatTail includeIdents

pFormatTail :: Bool -> TokenParser Format
pFormatTail includeIdents
  = pBase
    <??> ( pKeyDot **>
           (   pBase <**> ((\u b l -> Format_Tail l b u) <$> opt (pKeyDot **> pBase) Format_Empty)
           <|> (\u l -> Format_Tail l Format_Empty u) <$ pKeyDot <*> pBase
           ) )
  where pBase = pFormatBase includeIdents

pFormatBase :: Bool -> TokenParser Format
pFormatBase includeIdents
  =   Format_Identifier        <$> (if includeIdents then pIdentifier else pFail)
  <|> (Format_Primitive . snd) <$> pString
  <|> pPacked pKeyOParen pKeyCParen pToplevel
  <|> withIt Format_Primitive Format_Seq "{" "}" <$> pPacked pKeyOBrack pKeyCBrack pToplevel
  <|> withIt Format_Primitive Format_Seq "[" "]" <$> pPacked pKeyOSquar pKeyCSquar pToplevel
  where pToplevel = pFormat includeIdents

withIt :: (String -> a) -> (a -> a -> a) -> String -> String -> a -> a
withIt prim sq l r v = prim l `sq` (v `sq` prim r)

pRawInstance :: TokenParser (Pos, String)
pRawInstance
  = pSwitch fromTo pString
  where
    fromTo :: TokenState -> (TokenState, TokenState -> TokenState)
    fromTo (TokenState s errs unsafe scan pos st ok)
      = ( TokenState s errs unsafe scanLayout pos 0 ok
        , \(TokenState s' errs' unsafe' _ pos' _ ok') ->
            TokenState s' errs' unsafe' scan pos' st ok'
        )


-- Parsers for keywords
pKeyModule    = pKey "module"
pKeyType      = pKey "type"
pKeyAlt       = pKey "alt"
pKeyGeneral   = pKey "general"
pKeyExternal  = pKey "external"
pKeyRelation  = pKey "relation"
pKeyHole      = pKey "hole"
pKeyRule      = pKey "rule"
pKeyIdent     = pKey "ident"
pKeyPrem      = pKey "prem"
pKeyConc      = pKey "conc"
pKeyPattern   = pKey "pattern"
pKeyFormat    = pKey "format"
pKeyInfixl    = pKey "infixl"
pKeyInfixr    = pKey "infixr"
pKeyInfix     = pKey "infix"
pKeyBesides   = pKey "#"
pKeyAbove     = pKey "-"
pKeyNext      = pKey "^"

