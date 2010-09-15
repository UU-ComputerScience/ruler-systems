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
  compare a b =
   let
    c (DepMatch n)    (DepMatch m)     = compare m n
    -- c (DepMatch n)    (DepAssert m)    = compare m n
    c (DepMatch _)    _                = GT
    -- c (DepAssert n)   (DepMatch m)     = compare m n
    c (DepAssert n)   (DepMatch m)     = LT
    c (DepAssert n)   (DepAssert m)    = compare m n
    c (DepAssert _)   _                = GT
    c (DepDefault _)  (DepMatch _)     = LT
    c (DepDefault _)  (DepAssert _)    = LT
    c (DepDefault n)  (DepDefault m)   = compare m n
    c (DepDefault _)  _                = GT
    c (DepAttach _)   (DepMatch _)     = LT
    c (DepAttach _)   (DepAssert _)    = LT
    c (DepAttach _)   (DepDefault _)   = LT
    c (DepAttach n)   (DepAttach m)    = compare m n
    c (DepAttach _)   _                = GT
    c (DepInvoke _)   (DepMatch _)     = LT
    c (DepInvoke _)   (DepAssert _)    = LT
    c (DepInvoke _)   (DepDefault _)   = LT
    c (DepInvoke _)   (DepAttach _)    = LT
    c (DepInvoke n)   (DepInvoke m)    = compareNames (reverse n) (reverse m)
    c (DepInvoke _)   _                = GT
    c (DepVisStart _) (DepMatch _)     = LT
    c (DepVisStart _) (DepAssert _)    = LT
    c (DepVisStart _) (DepDefault _)   = LT
    c (DepVisStart _) (DepAttach _)    = LT
    c (DepVisStart _) (DepInvoke _)    = LT
    c (DepVisStart n) (DepVisStart m)  = compareNames (reverse n) (reverse m)
    c (DepVisStart _) _                = GT
    c (DepVisEnd n)   (DepVisEnd m)    = compareNames (reverse n) (reverse m)
    c (DepVisEnd _)   _                = LT
    c (DepClause _)   (DepMatch _)     = LT
    c (DepClause _)   (DepAssert _)    = LT
    c (DepClause _)   (DepDefault _)   = LT
    c (DepClause _)   (DepAttach _)    = LT
    c (DepClause _)   (DepInvoke _)    = LT
    c (DepClause _)   (DepVisStart _)  = LT
    c (DepClause _)   (DepVisEnd _)    = GT
    c (DepClause n)   (DepClause m)    = compareNames (reverse n) (reverse m)
   in c b a

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
            | ReasonOrder
  deriving (Eq,Ord,Show)
