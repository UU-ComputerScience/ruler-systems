module Util.Parsing.Token where

data Pos = Pos { line   :: !Int
               , column :: !Int
               , file   :: !String
               } deriving (Eq, Ord)

instance Show Pos where
  show (Pos l c f) = f ++ ":" ++ show l ++ "," ++ show c

advl ::  Int -> Pos ->Pos
advl i (Pos l c f) = (Pos (l+i) 1 f)

advc :: Int -> Pos ->  Pos
advc i (Pos l c f) = (Pos l (c+i) f)

adv :: Pos -> Char -> Pos
adv pos c = case c of
  '\t' -> advc 8 pos
  '\n' -> advl 1 pos
  _    -> advc 1 pos

noPos :: Pos
noPos = Pos (-1) (-1) ""

data Sym
  = SymReserved !String
  | SymIdent
  | SymString
  | SymInt
  deriving (Eq, Ord)

data Token
  = TkReserved !String !Pos
  | TkIdent    !String !Pos
  | TkString   !String !Pos
  | TkInt      !Int    !Pos
  | TkError    !String !Pos

getPos :: Token -> Pos
getPos (TkReserved _ p) = p
getPos (TkIdent _ p)    = p
getPos (TkString _ p)   = p
getPos (TkInt _ p)      = p
getPos (TkError _ p)    = p

instance Show Sym where
  show (SymReserved nm) = "reserved " ++ show nm
  show SymIdent         = "identifier"
  show SymString        = "string"
  show SymInt           = "integer"

instance Show Token where
  show (TkReserved nm p) = "reserved " ++ show nm
  show (TkIdent nm p)    = "identifier " ++ show nm
  show (TkString s p)    = "string " ++ show s
  show (TkInt i p)       = "integer " ++ show i
  show (TkError msg p)   = "error " ++ msg

type AlexInput = (Pos, String)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "alexInputPrevChar: cannot go back in input."

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar (_, []) = Nothing
alexGetChar (p, (c:cs))
  = let p' = adv p c
    in Just (c, (p', cs))

withSt :: Int -> (String -> Pos -> Token) -> String -> Pos -> (Int, Token)
withSt st tk s p = (st, tk s p) 

