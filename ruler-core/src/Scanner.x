{
module Scanner(tokenize, ppTokens) where

import UU.Scanner
import UU.Scanner.GenToken
import Data.Char
import UU.Pretty
import Opts
}

$litChar     = [^[\" \\]]
$identChar   = [a-zA-Z0-9\'_]

@ucIdent     = [A-Z] $identChar*
@lcIdent     = [a-z] $identChar*

tokens :-
  <0,a>     (" " | \n | \r)+                             ;                        -- ignore spaces
  <0,a,h>   "--" $white .*                               ;                        -- to-end-of-line comment
  <0,a>     "{-" ([^\-] | $white | "-" [^\}])* "-}"      ;                        -- multi-line comment (not nested)
  <h>       "{-" ([^\-] | $white | "-" [^\}])* "-}"      { valueToken TkTextln }  -- haskell comment token

  <0,h,j>  "{"                                        { reserved }
  <h,j>    "}"                                        { reserved }

  <0>    itf | visit | cyclic | inh | syn             { reserved }
  <0>    data | con | type | Maybe | ext              { reserved }
  <0>    datasem                                      { reserved }

  <a>    clause | visit | cyclic | chn | internal     { reserved }
  <a>    match | invoke | of                          { reserved }
  <a>    attach | detach | child                      { reserved }
  <a>    default | default1 | "default?"              { reserved }

  <0,a>  "::" | ":" | "monad"                         { reserved }
  <0,a>  var                                          { reserved }

  <0,a>  "(" | ")" | "[" | "]" | ","                  { reserved }
  <a>    "=" | "<-"                                   { reserved }
  <a>    "." | "@" | "_"                              { reserved }
  <h,j>  cosem | sem | datasem                        { reserved }

  <0,a>  @lcIdent                                     { valueToken TkVarid  }
  <0,a>  @ucIdent                                     { valueToken TkConid  }

  <h,j>  \" ([^\"]|\\\")* \"                          { valueToken TkTextln } -- scan string
  <h>    @ucIdent "." @lcIdent                        { valueToken TkTextln } -- treat qualified names as opaque strings
  <h>    @lcIdent "." @lcIdent                        { valueToken TkVarid  } -- scan attr occ
  <j>    @lcIdent ":" @lcIdent                        { valueToken TkVarid  } -- scan attr occ


{
tokenize :: Opts -> FilePath -> String -> [Token]
tokenize opts path str
  = merge $ scanTks opts [(Pos 1 0 path, 0)] (Pos 1 1 path, str)

merge []            = []
merge r@[_]         = r
merge (ValToken TkTextln s1 p : ValToken TkTextln s2 _ : r)
  = merge (ValToken TkTextln (s1 ++ s2) p : r)
merge (t : r)       = t : merge r

type AlexInput = (Pos, String)  -- current position, current string

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (_, [])     = Nothing
alexGetChar (p, c : cs) = Just (c, (adv p c, cs))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "alexInputPrevChar: should not be used."

type Ctx = (Pos, Int)  -- context start position, start code
type CtxStack = [Ctx]

scanTks :: Opts -> CtxStack -> AlexInput -> [Token]
scanTks opts st inp@(pos,str)
  = let (sc,st',tks) = unwind pos str st []
    in tks ++
       case alexScan inp sc of
         AlexEOF                -> unwindAll st' pos
         AlexError _            -> let (Just (c, inp')) = alexGetChar inp
                                       tk | sc == h || sc == j = valueToken TkTextln [c] pos
                                          | otherwise          = errToken [c] pos
                                   in tk : scanTks opts st' inp'
         AlexSkip inp' _        -> scanTks opts st' inp'
         AlexToken inp' len act -> let tk = act (take len str) pos
                                   in tk : scanTks opts (push opts sc tk st') inp'

push :: Opts -> Int -> Token -> CtxStack -> CtxStack
push opts sc (Reserved k pos)
  | sc == 0 && k == "{"          = ((noPos, n) :)
  | sc == h && k == "{"          = ((noPos, n) :)
  | sc == h && k == "}"          = tail
  | sc == j && k == "{"          = ((noPos, j) :)
  | sc == j && k == "}"          = tail
  | sc == 0 && k == "itf"        = ((pos, 0) :)
  | sc == 0 && k == "data"       = ((pos, 0) :)
  | sc /= a && k == "sem"        = ((pos, a) :)
  | sc /= a && k == "datasem"    = ((pos, a) :)
  | sc /= a && k == "cosem"      = ((pos, a) :)
  | sc /= a && k == "detach"     = ((pos, a) :)
  | sc == a && k == "="          = ((pos, n) :)
  | sc == a && k == "<-"         = ((pos, n) :)
  | sc == 0 && k == "::"         = ((pos, n) :)
  | sc == a && k == "::"         = ((pos, n) :)
  | sc == a && k == "monad"      = ((pos, n) :)
  | sc == a && k == "visit"      = ((pos, a) :)
  | sc == a && k == "internal"   = ((pos, a) :)
  | sc == a && k == "clause"     = ((pos, a) :)
  where
    n | genHaskell opts = h
      | genJs opts      = j
      | otherwise       = error "target language unspecified"
push _ _ _ = id

unwind :: Pos -> String -> CtxStack -> [Token] -> (Int, CtxStack, [Token])
unwind p str st@((p',c) : st') acc
  |    (cp == cp' &&
         (null str || (not $ isSpace $ head str)) )
    || (cp < cp' &&
         ( any (not . isSpace) $
             take (cp' - cp) str) ) = unwind p str st' (reserved "end" p : acc)
  | otherwise                       = (c, st, acc)
  where cp  = column p
        cp' = column p'

unwindAll :: CtxStack -> Pos -> [Token]
unwindAll st pos
  = map (const (reserved "end" pos)) (init st)

ppTokens :: [Token] -> PP_Doc
ppTokens = vlist . map ppToken

ppToken :: Token -> PP_Doc
ppToken (Reserved str pos)    = "keyword " >|< show str >|< " at " >|< ppPos pos
ppToken (ValToken tp val pos) = show tp >|< " " >|< val >|< " at " >|< ppPos pos

ppPos :: Pos -> PP_Doc
ppPos (Pos l c _) = show l >|< "," >|< show c
}
