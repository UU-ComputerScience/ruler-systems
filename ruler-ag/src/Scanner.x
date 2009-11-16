{
module Scanner(tokenize) where

import UU.Scanner
import UU.Scanner.GenToken
import Data.Char
import Debug.Trace
}

%wrapper "monad"

$litChar     = [^[\" \\]]
$identChar   = [a-zA-Z0-9\'_]

@ucIdent     = [A-Z] $identChar*
@lcIdent     = [a-z] $identChar*

tokens :-
  <0>  $white+                       ;
  <0>  "--" $white .*                ;

  <0>  interface | visit | static |
       inputs | outputs |
       sem | production |
       clause | external |
       child | match | eval          { tokenAct reserved }

  <0>  "=" | "::" | ":"              { tokenAct reserved }

  <0>  @lcIdent                      { tokenAct (valueToken TkVarid) }
  <0>  @ucIdent                      { tokenAct (valueToken TkConid) }

  <0>  "{" | "}"                     { tokenAct reserved `andBegin` h }

  <h>  "{" | "}"                     { tokenAct reserved `andBegin` 0 }
  <h>  [^@\n\}\{]+                   { tokenAct (valueToken TkString) }
  <h>  "@" @lcIdent "." @lcIdent     { tokenAct (valueToken TkVarid)  }
  <h>  "@" @lcIdent ":" @lcIdent     { tokenAct (valueToken TkConid)  }
  <h>  \n                            { tokenAct (valueToken TkString) }


{
alexEOF :: Alex Token
alexEOF = return $ reserved "eof" (Pos 0 0 somefile)

somefile :: String
somefile = "<some file>"

tokenize :: FilePath -> String -> Either String [Token]
tokenize filename contents
  = case runAlex contents (combine id) of
      Left err -> Left err
      Right f  -> Right (f [])
  where
    combine acc
      = do tk  <- errorWrap alexMonadScan
           c   <- alexGetStartCode
           case tk of
             Reserved s _ | s == "eof" -> return acc
             _                         -> combine (acc . (mapName tk :))

    mapName (Reserved k (Pos l c _))      = Reserved k (Pos l c filename)
    mapName (ValToken tp val (Pos l c _)) = ValToken tp val (Pos l c filename)


errorWrap :: Alex t -> Alex t
errorWrap (Alex r)
  = Alex $ \s -> case r s of
                   Left msg -> let (AlexPn _ l c) = alex_pos s
                               in Left (msg ++ " at " ++ show l ++ "." ++ show c)
                   Right v  -> Right v

tokenAct :: (String -> Pos -> Token) -> AlexAction (Alex Token)
tokenAct f (AlexPn _ l c,_,s) len
  = return (f s' p)
  where
    s' = take len s
    p  = Pos l c somefile
}
