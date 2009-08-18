{
module Scanner(tokenize) where

import UU.Scanner
import UU.Scanner.GenToken
}

$litChar     = [^[\" \\]]
$identChar   = [a-zA-Z0-9\'_]
$single      = [\(\)\;\{\}!]
$many        = [\,\.\:\[\]\-_=\>\<\|\$\#\&\^\~\*\+\%\/]

tokens :-
  $white+                                            ;                          -- whitespace
  "--" $white .*                                     ;                          -- comment

  \" ($litChar | \\ \\ | \\ \" )* \"                 { valueToken TkString }    -- string
  [0-9]+                                             { valueToken TkInteger10 } -- int
  
  syntax | keywords | augment                        { reserved }               -- syntax/keyword block

  ( derivation| external | merge | branch | order
  | inputs | outputs | visit | requires | exposes
  | hide | skip | abstract | innername
  | inst | as | establish | fresh | eval )           { reserved }               -- keywords
  ==                                                 { reserved }               -- equiv keyword
  \:=                                                { reserved }               -- bind keyword
  "-----" [\-]*                                      { reserved . take 5 }      -- horizontal line

  ( \\ | let | in | of | case| \-\> | = | @ | \|
  | invoke | result | if | then | else )             { reserved }               -- lambda calc.

  ( __invoke | __root | __app | __res | __this
  | __arg | __alt | __scrut | __call | __main )      { reserved }               -- reserved keywords

  $single                                            { reserved }               -- keyword symbols (single)
  $many+                                             { reserved }               -- keyword symbols (consecutive)

  [a-z] $identChar*                                  { valueToken TkVarid }     -- identifiers (lower case)
  [A-Z] $identChar*                                  { valueToken TkConid }     -- identifiers (upper case)


{
-- boilerplate code needed for Alex
type AlexInput = (Pos, String)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "alexInputPrevChar: there is no need to go back in the input."

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar (_, []) = Nothing
alexGetChar (p, (c:cs))
  = let p' = adv p c
    in Just (c, (p', cs))

-- use the Alex scanner to generate a list of tokens for the uulib token parsers
tokenize :: String -> String -> [Token]
tokenize filename str
  = go (initpos, str)
  where
    initpos = Pos 1 1 filename
    
    go inp@(pos, cs)
      = case alexScan inp 0 of
          AlexEOF         -> []
          AlexError inp'  -> valueToken TkError [head cs] pos : go (advc 1 pos, cs)
          AlexSkip inp' _ -> go inp'
          AlexToken inp' len act -> act (take len cs) pos : go inp'
}
