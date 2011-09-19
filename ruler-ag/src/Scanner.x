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
  <0,a>  $white+                       ;
  <0,a>  "--" $white .*                ;

  <0>    "{"                           { tokenAct reserved }
  <a>    "{" | "}}"                    { tokenAct reserved }
  <h>    "{" | "}"  | "{{"             { tokenAct reserved }

  <0>    interface | visit | forall |
         inputs | outputs              { tokenAct reserved }

  <a>    sem | production |
         static | visit |
         clause | external |
         child | match | eval          { tokenAct reserved }
  
  <0>    "."                           { tokenAct reserved }
  <0,a>  "::" | ":"                    { tokenAct reserved }

  <a>    "="                           { tokenAct reserved }

  <0,a>  @lcIdent                      { tokenAct (valueToken TkVarid)  }
  <0,a>  @ucIdent                      { tokenAct (valueToken TkConid)  }  

  -- poor Haskell lexing
  <h>    \" ([^\"]|\\\")* \"           { tokenAct (valueToken TkString) }
  <h>    [^@\}\{\"]+                   { tokenAct (valueToken TkString) }
  <h>    "@" @lcIdent "." @lcIdent     { tokenAct (valueToken TkVarid)  }
  <h>    "@" @lcIdent ":" @lcIdent     { tokenAct (valueToken TkConid)  }
  <h>    \r\n | \n                     { tokenAct (valueToken TkString) }


{
alexEOF :: Alex Token
alexEOF = return $ reserved "eof" (Pos 0 0 somefile)

somefile :: String
somefile = "<some file>"

data Ctx
  = InH
  | InA
  deriving Show

getStartCode []      = 0
getStartCode (InH:_) = h
getStartCode (InA:_) = a

tokenize :: FilePath -> String -> Either String [Token]
tokenize filename contents
  = case runAlex contents (combine [] id) of
      Left err -> Left err
      Right f  -> Right (f [])
  where
    combine stack acc
      | seq (seq stack acc) True
      = do alexSetStartCode (getStartCode stack)
           tk   <- errorWrap alexMonadScan
           let acc' = acc . (mapName tk :)
           case tk of
             Reserved s _ | s == "eof" -> return acc
                          | s == "{"   -> combine (push InH stack) acc'
                          | s == "}"   -> combine (pop stack)      acc'
                          | s == "{{"  -> combine (push InA stack) acc'
                          | s == "}}"  -> combine (pop stack)      acc'
             _                         -> combine stack            acc'

    push       = (:)
    pop []     = []
    pop (_:xs) = xs

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
