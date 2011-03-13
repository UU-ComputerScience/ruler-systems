{
-- Lexical syntax for the Base Ruler Language
module Base.Scanner(nextToken) where

import Util.Parsing.Token
}

$litChar     = [^[\" \\]]
$identChar   = [a-zA-Z0-9\'_]
$symbols     = [=\,\:\{\}\(\)\-\#\^]
$embeddedSym = [\{\}\(\)\[\]]
$embeddedOp  = [=\-_\'\>\<\|\/\,\:\;]

tokens :-
  <0,format,pattern>  $white+                                      ;                              -- whitespace
  <0,format,pattern>  "--" $white .*                               ;                              -- comment

  <0>                 \" ($litChar | \\ \\ | \\ \" )* \"           { withSt 0 (TkString . read) }       -- string
  <format>            \" ($litChar | \\ \\ | \\ \" )* \"           { withSt format (TkString . read) }  -- string
  <pattern>           \" ($litChar | \\ \\ | \\ \" )* \"           { withSt format (TkString . read) }  -- string

  <0,format,pattern>  format                                       { withSt format TkReserved }   -- keyword, switch to scan formats
  <0,format,pattern>  pattern                                      { withSt pattern TkReserved }  -- pattern, switch to scan patterns
  <0,format,pattern>  ( module
                      | type | general | alt | external
                      | relation | hole | rule
                      | ident | prem | conc )                      { withSt 0 TkReserved }        -- keyword, switch back to general scan

  <0>                 \.                                           { withSt 0 TkReserved }        -- point
  <format>            \.                                           { withSt format TkReserved }   -- point
  <pattern>           \.                                           { withSt pattern TkReserved }  -- point

  <0>                 $symbols+                                    { withSt 0 TkReserved }        -- symbols
  <format>            $embeddedSym                                 { withSt format TkReserved }   -- symbols (during format)
  <format>            $embeddedOp+                                 { withSt format TkString }     -- operators (during format)
  <pattern>           $embeddedSym                                 { withSt pattern TkReserved }  -- symbols (during pattern)
  <pattern>           [\\ $embeddedOp]+                            { withSt pattern TkString }    -- operators (during pattern)

  <pattern>           ( infixl | infixr | infix )                  { withSt pattern TkReserved }  -- fixity operators

  <0>                 [a-zA-Z] $identChar* (\. $identChar+)*       { withSt 0 TkIdent }           -- identifier
  <format>            [a-zA-Z] $identChar* (\. $identChar+)*       { withSt format TkIdent }      -- identifier
  <pattern>           [a-zA-Z] $identChar* (\. $identChar+)*       { withSt pattern TkIdent }     -- identifier

  <format>            \\ [a-zA-Z] $identChar*                      { withSt format TkString }     -- LaTeX symbol  
  <pattern>           [0-9]+                                       { withSt pattern (\s -> TkInt (read s)) }  -- operator priority

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

