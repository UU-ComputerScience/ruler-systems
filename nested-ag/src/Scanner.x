{
module Scanner(tokenize, ppTokens) where

import UU.Scanner
import UU.Scanner.GenToken
import Data.Char
import UU.Pretty
}

$litChar     = [^[\" \\]]
$identChar   = [a-zA-Z0-9\'_]

@ucIdent     = [A-Z] $identChar*
@lcIdent     = [a-z] $identChar*

tokens :-
  <0,a>   $white+                                     ;                        -- ignore spaces
  <0,a>   "--" $white .*                              ;                        -- to-end-of-line comment
  <0,a> "{-" ([^\-] | $white | "-" [^\}])* "-}"       ;                        -- multi-line comment (not nested)
  <h>   "{-" ([^\-] | $white | "-" [^\}])* "-}"       { valueToken TkTextln }  -- haskell comment token

  <0>    "{"                                          { reserved }
  <h>    "}"                                          { reserved }

  <0>    itf | sem | visit | inh | syn                { reserved }

  <a>    visit | child | invoke                       { reserved }

  <0,a>  "::"                                         { reserved }

  <a>    "=" | "."                                    { reserved }
  <h>    sem                                          { reserved }

  <0,a>  @lcIdent                                     { valueToken TkVarid  }
  <0,a>  @ucIdent                                     { valueToken TkConid  }

  <h>    \" ([^\"]|\\\")* \"                          { valueToken TkTextln } -- scan string
  <h>    @ucIdent "." @lcIdent                        { valueToken TkTextln } -- treat qualified names as opaque strings
  <h>    @lcIdent "." @lcIdent                        { valueToken TkVarid  } -- scan attr occ


{
tokenize :: FilePath -> String -> [Token]
tokenize path str
  = merge $ scanTks [(Pos 1 0 path, 0)] (Pos 1 1 path, str)

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

scanTks :: CtxStack -> AlexInput -> [Token]
scanTks st inp@(pos,str)
  = let (sc,st',tks) = unwind pos str st []
    in tks ++
       case alexScan inp sc of
         AlexEOF                -> unwindAll st' pos
         AlexError _            -> let (Just (c, inp')) = alexGetChar inp
                                       tk | sc == h   = valueToken TkTextln [c] pos
                                          | otherwise = errToken [c] pos
                                   in tk : scanTks st' inp'
         AlexSkip inp' _        -> scanTks st' inp'
         AlexToken inp' len act -> let tk = act (take len str) pos
                                   in tk : scanTks (push sc tk st') inp'

push :: Int -> Token -> CtxStack -> CtxStack
push sc (Reserved k pos)
  | sc == 0 && k == "{"          = ((noPos, h) :)
  | sc == h && k == "}"          = tail
  | sc == 0 && k == "itf"        = ((pos, 0) :)
  | sc /= a && k == "sem"        = ((pos, a) :)
--  | sc == a && k == "child"      = ((pos, a) :)
  | sc == a && k == "="          = ((pos, h) :)
  | sc == 0 && k == "::"         = ((pos, h) :)
push _ _ = id

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
