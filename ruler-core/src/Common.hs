module Common
  ( Ident(..), identName, identPos, ident, explainIdent, explainPos, QIdent
  , module UU.Scanner.Position
  , DepItem(..), Reason(..)
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


--
-- DepItems
--

data DepItem
  = DepMatch      !Int     -- occ id
  | DepAssert     !Int     -- occ id
  | DepDefault    !Int     -- code id
  | DepAttach     !Int     -- occ id
  | DepInvoke     !QIdent  -- name : qual childname
  | DepVisStart   !QIdent  -- name : basename
  | DepVisEnd     !QIdent  -- name : basename
  | DepClause     !QIdent  -- name : basename (or: loc.basename)
  deriving (Eq, Show)

instance Ord DepItem where
  compare (DepMatch n)    (DepMatch m)     = compare m n
  compare (DepMatch n)    (DepAssert m)    = compare m n
  compare (DepMatch _)    _                = GT
  compare (DepAssert n)   (DepMatch m)     = compare m n
  compare (DepAssert n)   (DepAssert m)    = compare m n
  compare (DepAssert _)   _                = GT
  compare (DepDefault _)  (DepMatch _)     = LT
  compare (DepDefault _)  (DepAssert _)    = LT
  compare (DepDefault n)  (DepDefault m)   = compare m n
  compare (DepDefault _)  _                = GT
  compare (DepAttach _)   (DepMatch _)     = LT
  compare (DepAttach _)   (DepAssert _)    = LT
  compare (DepAttach _)   (DepDefault _)   = LT
  compare (DepAttach n)   (DepAttach m)    = compare m n
  compare (DepAttach _)   _                = GT
  compare (DepInvoke _)   (DepMatch _)     = LT
  compare (DepInvoke _)   (DepAssert _)    = LT
  compare (DepInvoke _)   (DepDefault _)   = LT
  compare (DepInvoke _)   (DepAttach _)    = LT
  compare (DepInvoke n)   (DepInvoke m)    = compareNames (reverse n) (reverse m)
  compare (DepInvoke _)   _                = GT
  compare (DepVisStart _) (DepMatch _)     = LT
  compare (DepVisStart _) (DepAssert _)    = LT
  compare (DepVisStart _) (DepDefault _)   = LT
  compare (DepVisStart _) (DepAttach _)    = LT
  compare (DepVisStart _) (DepInvoke _)    = LT
  compare (DepVisStart n) (DepVisStart m)  = compareNames (reverse n) (reverse m)
  compare (DepVisStart _) _                = GT
  compare (DepVisEnd n)   (DepVisEnd m)    = compareNames (reverse n) (reverse m)
  compare (DepVisEnd _)   _                = LT
  compare (DepClause _)   (DepMatch _)     = LT
  compare (DepClause _)   (DepAssert _)    = LT
  compare (DepClause _)   (DepDefault _)   = LT
  compare (DepClause _)   (DepAttach _)    = LT
  compare (DepClause _)   (DepInvoke _)    = LT
  compare (DepClause _)   (DepVisStart _)  = LT
  compare (DepClause _)   (DepVisEnd _)    = GT
  compare (DepClause n)   (DepClause m)    = compareNames (reverse n) (reverse m)

compareNames :: QIdent -> QIdent -> Ordering
compareNames [] [] = EQ
compareNames [] _  = GT
compareNames _  [] = LT
compareNames (x:xs) (y:ys)
  = case compare x y of
      LT -> GT
      GT -> LT
      EQ -> compareNames xs ys

-- Dependency reasons
data Reason = ReasonScopeVisit !Ident
            | ReasonScopeClause !Ident
            | ReasonScopeEnd !Ident
            | ReasonAttrReq !Ident !Ident
            | ReasonInvokeReq !Ident
            | ReasonChildReq !Ident
            | ReasonAttach !Ident !Ident
            | ReasonDefault !Ident
            | ReasonDetach !Ident !Ident
            | ReasonSink
            | ReasonAlloc
            | ReasonError
  deriving (Eq,Ord,Show)
