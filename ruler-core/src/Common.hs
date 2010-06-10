module Common
  ( Ident(..), identName, identPos, ident, explainIdent, explainPos, QIdent
  , module UU.Scanner.Position
  )
  where

import UU.Scanner.Position
import Data.Map(Map)
import qualified Data.Map as Map


--
-- Identifiers
--

data Ident
  = Ident String Pos

identName :: Ident -> String
identName (Ident nm _) = nm

identPos :: Ident -> Pos
identPos (Ident _ pos) = pos

ident :: String -> Ident
ident nm = Ident nm noPos

instance Show Ident where
  show (Ident nm _) = nm

instance Eq Ident where
  (Ident nmA _) == (Ident nmB _) = nmA == nmB

instance Ord Ident where
  compare (Ident nmA _) (Ident nmB _) = compare nmA nmB

explainIdent :: Ident -> String
explainIdent ident = show ident ++ " {" ++ explainPos (identPos ident) ++ "}"

explainPos :: Pos -> String
explainPos (Pos l c file) = file ++ ":" ++ show l ++ "," ++ show c

--
-- Qualified Identifiers
--

type QIdent = [Ident]
