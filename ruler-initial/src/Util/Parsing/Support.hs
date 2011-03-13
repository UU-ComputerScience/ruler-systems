{-# OPTIONS_GHC -fglasgow-exts #-}
module Util.Parsing.Support where

import Util.Parsing.Parser
import Util.Parsing.Token
import Data.Set(Set)
import qualified Data.Set as Set

infixl  5  <$>
(<$>) = (<$$>)


--
-- Stream of characters that encapsulates a scanner to hand tokens to the
-- parsers.
--

type Errors = [Error Token Sym Pos]
type UnsafeSyms = Set Sym
type Scanner = Int -> Pos -> String -> Maybe (Token, Int, String, Pos, Int) 
data TokenState = TokenState String Errors UnsafeSyms Scanner !Pos !Int !Bool

instance Provides TokenState Sym Token where
  splitState sym cont (TokenState str msgs unsafe scan pos st ok)
    = case test of
        Just _  -> if sym `eqSymTok` t
                   then Step prog (cont t (TokenState str' msgs unsafe scan pos' st' ok))
                   else failure ok
        Nothing -> failure False
    where
      test@(~(Just (t, prog, str', pos', st'))) = scan st pos str
      
      failure b = Fail [show sym] (ins:if b then [del] else [])
      ins exp = Just (symCost sym, cont (mkTok sym)    (TokenState str  (msgs ++ [Inserted sym pos exp]) unsafe scan pos  st  False))
      del exp = Just (5, splitState sym cont (TokenState str' (msgs ++ [Deleted t pos exp])    unsafe scan pos' st' True))

      eqSymTok (SymReserved n) (TkReserved m _) = n == m
      eqSymTok SymIdent        (TkIdent _ _)   = True
      eqSymTok SymString       (TkString _ _)  = True
      eqSymTok SymInt          (TkInt _ _)     = True
      eqSymTok _               _               = False

      symCost s | s `Set.member` unsafe = 99
      symCost (SymReserved nm)          = 5
      symCost SymString                 = 8
      symCost SymIdent                  = 8
      symCost SymInt                    = 8

      mkTok SymIdent        = TkIdent "x" pos
      mkTok SymString       = TkString "" pos
      mkTok SymInt          = TkInt 0 pos
      mkTok (SymReserved s) = TkReserved s pos

instance Eof TokenState where
  eof (TokenState str _ _ scan pos st _)
    = maybe True (const False) (scan st pos str)

  deleteAtEnd (TokenState str msgs unsafe scan pos st ok)
    = maybe Nothing (\(t, _, str', pos', st') ->
                      Just (5, TokenState str' (msgs ++ [DeletedAtEnd t]) unsafe scan pos' st' ok))
    $ scan st pos str

instance Stores TokenState Errors where
  getErrors (TokenState ts msgs unsafe scan pos st ok)
    = (msgs, TokenState ts [] unsafe scan pos st ok)


--
-- Interface
--

type TokenParser a = P_m TokenState a

run :: Pos -> TokenParser a -> Scanner -> Set Sym -> String -> (a, Errors)
run pos p scan unsafe ts
  = parse ((,) <$> p <*> pEnd) (TokenState ts [] unsafe scan pos 0 True)


--
-- Derived combinators
--

pKey :: String -> TokenParser Pos
pKey k = (\(TkReserved _ p) -> p) <$> pSym (SymReserved k)

pIdent :: TokenParser (Pos, String)
pIdent = (\(TkIdent s p) -> (p, s)) <$> pSym SymIdent

pString :: TokenParser (Pos, String)
pString = (\(TkString s p) -> (p, s)) <$> pSym SymString

pInt :: TokenParser (Pos, Int)
pInt = (\(TkInt i p) -> (p, i)) <$> pSym SymInt

infixl  5  <$
infixl  5  <**
infixl  5  **>

(<$) :: b -> TokenParser a -> TokenParser b
(<$) f p = const f <$> p

(<**) :: TokenParser a -> TokenParser b -> TokenParser a
(<**) p q = const <$> p <*> q

(**>) :: TokenParser b -> TokenParser a -> TokenParser a
(**>) p q = flip const <$> p <*> q

opt :: TokenParser a -> a -> TokenParser a
opt p v = p <<|> pReturn v 

pList :: TokenParser b -> TokenParser [b]
pList p
  = rec where rec = (:) <$> p <*> rec <|> pReturn []

pListSep :: TokenParser b -> TokenParser b -> TokenParser [b]
pListSep sep p
  = rec <<|> pReturn []
  where rec = (:) <$> p <** sep <*> rec
              <|> (return <$> p)

pSequence :: [TokenParser a] -> TokenParser [a]
pSequence parsers
  = foldr (\p r -> (:) <$> p <*> r) (pReturn []) parsers

pChainr :: TokenParser (a -> a -> a) -> TokenParser a -> TokenParser a
pChainr op base
  = rec where rec = base <??> (flip <$> op <*> rec)

pChainl :: TokenParser (a -> a -> a) -> TokenParser a -> TokenParser a
pChainl op base = pChainr ((\o v r f -> r ((f $ v $ id) `o`)) <$> op)
                          (flip ($) <$> base) <*> pReturn id

pPacked :: TokenParser a -> TokenParser b -> TokenParser c -> TokenParser c
pPacked l r p = l **> p <** r

(<**>) :: TokenParser a -> TokenParser (a -> b) -> TokenParser b
p <**> q = flip ($) <$> p <*> q

(<$$$>) :: (a -> b -> c) -> TokenParser b -> TokenParser (a -> c)
f <$$$> p = flip f <$> p

(<??>) :: TokenParser a -> TokenParser (a -> a) -> TokenParser a
p <??> q = p <**> (q `opt` id)


--
-- Some common symbols
--

pKeyOBrack = pKey "{"
pKeyCBrack = pKey "}"
pKeyOParen = pKey "("
pKeyCParen = pKey ")"
pKeyOSquar = pKey "["
pKeyCSquar = pKey "]"
pKeyDColon = pKey "::"
pKeyEquals = pKey "="
pKeyComma  = pKey ","
pKeyDot    = pKey "."

