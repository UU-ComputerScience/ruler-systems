{
-- Lexical syntax for the concrete syntax of judgements
module Base.FlexScanner(nextToken) where

import Util.Parsing.Token
}

$litChar   = [^[\" \\]]
$identChar = [a-zA-Z0-9\'_]
$symbols   = [=\:\-\#\^\'_\>\<\|\/\;\-\#\^]
$single    = [\.\,\{\}\(\)\[\]]

tokens :-
  $white+                                      ;                              -- whitespace

  \" ($litChar | \\ \\ | \\ \" )* \"           { withSt 0 (TkString . read) } -- string

  [0-9]+                                       { withSt 9 (TkInt . read) }    -- number

  $single                                      { withSt 0 TkReserved }        -- single symbols
  $symbols+                                    { withSt 0 TkReserved }        -- symbols (grouped)

  [a-zA-Z] $identChar* (\. $identChar+)*       { withSt 0 TkIdent }           -- identifier

{
nextToken :: Int -> Pos -> String -> Maybe (Token, Int, String, Pos, Int)
nextToken stateId
  = curry go 
  where
    go inp@(p,cs) =
      case alexScan inp stateId of
        AlexEOF -> Nothing
        AlexError _ ->
          let (c:cs') = cs
              p' = adv p c
          in Just (TkError ("unexpected: " ++ [c]) p, 1, cs', p', stateId)
        AlexSkip inp' _ -> go inp'
        AlexToken inp' len act -> Just
          ( let (stateId', tk) = act (take len cs) p 
            in (tk, len, snd inp', fst inp', stateId') )
}

