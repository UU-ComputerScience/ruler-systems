module SyntaxExt(preprocess, SynExtensions(SynExtensions),SynExt(..),LamSym(..),EqSym(..)) where

import UU.Scanner.GenToken
import UU.Scanner.Token
import UU.Scanner.Position
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List
import RulerExpr


extraKeywords :: [Token] -> Set String
extraKeywords (Reserved s _ : tks)
      | s == "keywords" = let (inside,rest) = brackets tks
                          in Set.fromList (scanIdents inside) `Set.union` extraKeywords rest
extraKeywords (_ : tks) = extraKeywords tks
extraKeywords []        = Set.empty

brackets :: [Token] -> ([Token],[Token])
brackets (t : tks)
  | isKeyword "{" t = let (l,r) = break (isKeyword "}") tks
                      in if null r
                         then ([], t : tks)
                         else (l, tail r)
brackets tks        = ([], tks)

isKeyword :: String -> Token -> Bool
isKeyword nm (Reserved s _) | s == nm = True
isKeyword _  _                        = False

scanIdents :: [Token] -> [String]
scanIdents = concatMap match
  where match (ValToken TkVarid v _) = [v]
        match (ValToken TkConid v _) = [v]
        match _                      = []


-- Turns identifiers that match the names in the set into keywords
retokenize :: Set String -> [Token] -> [Token]
retokenize extra (ValToken TkVarid v pos : tks) | v `Set.member` extra = Reserved v pos : retokenize extra tks
retokenize extra (ValToken TkConid v pos : tks) | v `Set.member` extra = Reserved v pos : retokenize extra tks
retokenize extra (t : tks)                                             = t : retokenize extra tks
retokenize _ []                                                        = []


-- Representation of extra syntax rules. There are two variations:
--  LamExt: syntax for a function call.
--  EqExt: syntax for field assignments.
data SynExt = LamExt !Ident ![LamSym] | EqExt !Pos ![EqSym]
data LamSym = LamSymKey !String | LamSymParam !Int | LamSymParenParam !Int
data EqSym  = EqSymKey !String | EqSymInput !String | EqSymParenInput !String | EqSymOutput !String | EqSymParenOutput !String

extraSyntax :: [Token] -> [SynExt]
extraSyntax (Reserved s pos : tks)
  | s == "syntax" = case tks of
                      ValToken TkVarid v pos' : tks' -> let (inside, rest) = brackets tks'
                                                        in if null inside
                                                           then extraSyntax rest
                                                            else LamExt (Ident v pos') (toNumSymbols inside) : extraSyntax rest
                      _                           -> let (inside, rest) = brackets tks
                                                     in if null inside
                                                        then extraSyntax rest
                                                        else EqExt pos (toVarSymbols inside) : extraSyntax rest
extraSyntax (_ : tks) = extraSyntax tks
extraSyntax []        = []

toNumSymbols :: [Token] -> [LamSym]
toNumSymbols (t1 : ValToken TkInteger10 v _ : t2 : tks)
  | isKeyword "(" t1 && isKeyword ")" t2            = LamSymParenParam (read v) : toNumSymbols tks
toNumSymbols (ValToken TkInteger10 v _ : tks)       = LamSymParam (read v) : toNumSymbols tks
toNumSymbols (Reserved v _ : tks)                   = LamSymKey v : toNumSymbols tks
toNumSymbols (_ : tks)                              = toNumSymbols tks
toNumSymbols []                                     = []

toVarSymbols :: [Token] -> [EqSym]
toVarSymbols (t1 : ValToken TkVarid v _ : t2 : tks)
  | isKeyword "(" t1 && isKeyword ")" t2            
                                  = EqSymParenInput v : toVarSymbols tks
toVarSymbols (ValToken TkVarid v _ : tks)   
                                  = EqSymInput v : toVarSymbols tks
toVarSymbols (t1 : t2 : ValToken TkVarid v _ : t3 : tks)
  | isKeyword "(" t1 && isKeyword "!" t2 && isKeyword ")" t3
                                  = EqSymParenOutput v : toVarSymbols tks
toVarSymbols (t1 : ValToken TkVarid v _ : tks)
  | isKeyword "!" t1              = EqSymOutput v : toVarSymbols tks
toVarSymbols (Reserved v _ : tks) = EqSymKey v : toVarSymbols tks
toVarSymbols (_ : tks)            = toVarSymbols tks
toVarSymbols []                   = []


data SynExtensions = SynExtensions ![SynExt] ![String]

preprocess :: [Token] -> ([Token], SynExtensions)
preprocess tks
  = (tks', SynExtensions exts (Set.toList keys))
  where
    keys = extraKeywords tks
    tks' = retokenize keys tks
    exts = extraSyntax tks'

