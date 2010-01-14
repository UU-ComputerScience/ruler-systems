module Common where

import UU.Scanner.Position
import Data.Map(Map)
import qualified Data.Map as Map
import UU.Pretty


--
-- Identifiers
--

type Idents = [Ident]

data Ident
  = Ident String Pos

identName :: Ident -> String
identName (Ident nm _) = nm

identPos :: Ident -> Pos
identPos (Ident _ pos) = pos

ident :: String -> Ident
ident nm = Ident nm noPos

unknownIdent :: Ident
unknownIdent = Ident "<unknown>" noPos

instance Show Ident where
  show (Ident nm _) = nm

instance Eq Ident where
  (Ident nmA _) == (Ident nmB _) = nmA == nmB

instance Ord Ident where
  compare (Ident nmA _) (Ident nmB _) = compare nmA nmB

instance Eq Pos where
  (Pos a b c) == (Pos d e f) = a == d && b == e && c == f

instance Ord Pos where
  compare (Pos a b c) (Pos d e f) = case compare a d of
                                      EQ -> case compare b e of
                                              EQ -> compare c f
                                              x  -> x
                                      x  -> x

explainIdent :: Ident -> String
explainIdent ident = show ident ++ " {" ++ explainPos (identPos ident) ++ "}"

explainPos :: Pos -> String
explainPos (Pos l c file) = file ++ ":" ++ show l ++ "," ++ show c

instance PP Ident where
  pp (Ident nm _) = pp nm


--
-- Kind of blocks
--

data BlockKind
  = BlockKind_Code
  | BlockKind_Import
  | BlockKind_Preamble
  | BlockKind_Inline
  deriving (Eq, Ord, Show)

instance PP BlockKind where
  pp BlockKind_Code     = pp "code"
  pp BlockKind_Import   = pp "import"
  pp BlockKind_Preamble = pp "preamble"
  pp BlockKind_Inline   = pp "inline"

isInline :: BlockKind -> Bool
isInline BlockKind_Inline = True
isInline _                = False


--
-- Kind of bindings
--

data BindKind
  = BindKind_Assert
  | BindKind_Def
  deriving (Eq, Ord, Show)

instance PP BindKind where
  pp BindKind_Assert = pp "assert"
  pp BindKind_Def    = pp "bind"


--
-- Pretty print common helper functions
--

hlist_sep :: (PP a, PP b) => a -> [b] -> PP_Doc
hlist_sep s []     = empty
hlist_sep s [x]    = pp x
hlist_sep s (x:xs) = x >|< s >|< hlist_sep s xs

vlist_sep :: (PP a, PP b) => a -> [b] -> PP_Doc
vlist_sep s []     = empty
vlist_sep s [x]    = pp x
vlist_sep s (x:xs) = x >-< s >-< vlist_sep s xs

ppPos :: Pos -> PP_Doc
ppPos (Pos l c f) = pp "--" >#< f >|< ":" >|< l >|< "," >|< c

ppMbNm :: Maybe Ident -> PP_Doc
ppMbNm Nothing = empty
ppMbNm (Just x) = " " >|< x
