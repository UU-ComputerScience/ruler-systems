module Util.Common where

import Data.Char(isSpace)
import Util.Parsing.Parser
import Util.Parsing.Support
import Util.Parsing.Token


data Identifier = Ident { identName :: !String
                        , identPos  :: !Pos
                        }

instance Eq Identifier where
  (Ident a _) == (Ident b _) = a == b

instance Ord Identifier where
  compare (Ident a _) (Ident b _) = compare a b

instance Show Identifier where
  show (Ident a _) = a

relationNm :: Identifier
relationNm = identFromString "_rel"

typeNm :: Identifier
typeNm = identFromString "_tp"

identFromString :: String -> Identifier
identFromString nm = Ident nm noPos

identIdent :: Identifier
identIdent = identFromString "Ident"

identInt :: Identifier
identInt = identFromString "Int"

identString :: Identifier
identString = identFromString "String"


data Fixity
  = Infix_Left
  | Infix_Right
  | Infix
  deriving (Eq, Ord, Show)


-- |parses an arbitrary symbol, not containing ')'
pIdentSymbol :: TokenParser Identifier
pIdentSymbol
  = pSwitch fromTo (uncurry (flip Ident) <$> pIdent)
  where
    fromTo :: TokenState -> (TokenState, TokenState -> TokenState)
    fromTo (TokenState s errs unsafe scan pos st ok)
      = ( TokenState s errs unsafe scanAnySymbol pos 0 ok
        , \(TokenState s' errs' unsafe' _ pos' _ ok') ->
            TokenState s' errs' unsafe' scan pos' st ok'
        )

    scanAnySymbol :: Scanner
    scanAnySymbol st _ [] = Nothing
    scanAnySymbol st pos s@(c:cs)
      | isSpace c = scanAnySymbol st (adv pos c) cs
      | otherwise = Just ( let (sym, rest) = break (\c -> isSpace c || isTerminator c) s
                               n = length sym
                           in (TkIdent sym pos, n, rest, advc n pos, st)
                         )

    isTerminator :: Char -> Bool
    isTerminator c = c `elem` ")(\"\'"


-- |scans any prefix until the end of the string or until the first occurrence of any
--  of the given keywords (isolated through whitespace).
scanRaw :: [String] -> Scanner
scanRaw untilKeywords st pos
  = maybe Nothing (\(s, r, p) -> Just (TkString s pos, length s, r, p, st)) . scan id pos
  where
    scan acc p ""
      | null str  = Nothing
      | otherwise = Just (str, "", p)
      where str = acc ""
    scan acc p str
      = let (ss, r1)      = span isSpace str
            (ns, r2)      = break isSpace r1
            proceed a r f = scan (acc . (a ++)) (f $ advs p a) r
            advs          = foldl adv
        in if null ss
           then proceed ns r2 id
           else if ns == "--"
                then let (cs, r3) = break (\c -> c == '\r' || c == '\n') r2
                     in proceed ss r3 (advc (length cs))
                else if any (== ns) untilKeywords
                     then Just (acc ss, r1, advs p ss)
                     else proceed (ss ++ ns) r2 id


-- |scans until:
--  * the end of the string
--  * a non-whitespace character on a horizontal position before the first non-whitespace
--    character on the second line from the start of scanning
--  returns the scanned contents as one string
scanLayout :: Scanner
scanLayout st pos s
  = case lines s of
      []     -> Just (TkString "" pos, 0, "", pos, st)
      (l:ls) -> let firstNonwhite = length . dropWhile isSpace
                    h  = case dropWhile (== 0) $ map firstNonwhite ls of
                           []    -> 0
                           (x:_) -> x
                    (ls1,ls2) = span (\l -> let h' = firstNonwhite l in h' == 0 || h' >= h) ls
                    l' = unlines (l:ls1)
                in Just (TkString l' pos, length l', unlines ls2, advl (1+length ls1) pos, st)

