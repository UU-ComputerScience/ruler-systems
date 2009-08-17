{-# OPTIONS_GHC -fglasgow-exts -XScopedTypeVariables -XOverlappingInstances -XFlexibleContexts -XIncoherentInstances -XUndecidableInstances #-}
module Util where

import RulerExpr
import Text.PrettyPrint
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.IntSet(IntSet)
import qualified Data.IntSet as IntSet
import Data.Typeable
import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import UU.Scanner.Position
import Data.List
import Opts
import Dot
import Text.Html(Html,(+++))
import qualified Text.Html as H
import qualified Data.Graph as G


--
-- DerivationTree
--

data DerivationTree
  = DTNode { dtUid      :: Guess
           , dtTitle    :: Ident
           , dtContext  :: String
           , dtInputs   :: [(Ident,Value)]
           , dtOutputs  :: [(Ident,Value)]
           , dtVisits   :: [DerivationVisit]
           , dtStatus   :: Status
           }
  | DTLeaf { dtUid      :: Guess
           , dtValue    :: Value
           }

data DerivationVisit
  = DTVisit { dvName     :: Ident
            , dvSubtrees :: [(Ident, Guess, DerivationTree)]
            , dvMessages :: [Message]
            }

instance Show DerivationTree where
  show t  = render (toDoc (ident "root") t)

toDoc :: Ident -> DerivationTree -> Doc
toDoc name (DTNode _ title context inputs outputs visits status)
  = (text (show name) <+> text (show $ identPos name) <+> text "branch" <+> text (show title) <+> extra)
    $+$ nest 2 (   ( if null (concatMap dvSubtrees visits)
                     then empty
                     else text "subderivations"
                          $+$ nest 2 ( vcat [ toDoc k t | (k,_,t) <- concatMap dvSubtrees visits ] )
                   )
               $+$ ( if null inputs
                     then empty
                     else text "inputs"
                          <+> nest 2 ( vcat [ text (show k) <+> text "=" <+> text (show v) | (k,v) <- inputs ] )
                   )
               $+$ ( if null outputs
                     then empty
                     else text "outputs"
                          <+> nest 2 ( vcat [ text (show k) <+> text "=" <+> text (show v) | (k,v) <- outputs ] )
                   )
               $+$ ( if null (concatMap dvMessages visits)
                     then empty
                     else text "messages"
                          <+> nest 2 ( vcat (map (text . show) $ concatMap dvMessages visits))
                   )
               )
  where extra = case status of
                  Success   -> empty
                  Failure s -> text ("failed " ++ s)
toDoc name (DTLeaf _ val)
  = text (show name) <+> text "value" <+> text (show val)


defDocWidth :: Int
defDocWidth = 140

-- TT monad version
toDocTT :: Monad m => Int -> Ident -> DerivationTree -> TTM m Doc
toDocTT level name (DTNode _ title context inputs outputs visits status)
  = do visitDocs  <- mapM mkVisitDoc visits
       inputDocs  <- mapM createDoc inputs
       outputDocs <- mapM createDoc outputs
       return ( (text (show name) <+> text (show $ identPos name) <+> text "branch" <+> text (show title))
                $+$ ( case status of
                        Success   -> empty
                        Failure s -> text "FAILED"
                                     $+$ nest 2 (mkLines s)
                    )
                $+$ ( if null inputs
                      then empty
                      else text "inputs"
                           $+$ nest 2 ( vcat [ text (show k) <+> text "=" <+> d | ((k,_), d) <- zip inputs inputDocs ] )
                    )
                $+$ ( if null outputs
                      then empty
                      else text "outputs"
                           $+$ nest 2 ( vcat [ text (show k) <+> text "=" <+> d | ((k,_), d) <- zip outputs outputDocs ] )
                    )
                $+$ vcat visitDocs
              )
  where
    mkVisitDoc (DTVisit nm subtrees messages)
      = do subTreeDocs <- mapM (\(k,_,t) -> toDocTT (1+level) k t) subtrees
           return ( text ("visit " ++ show nm)
                        $+$ ( if null messages
                              then empty
                              else text "messages"
                                   $+$ nest 2 ( vcat (map (mkLines . show) messages) )
                            )
                        $+$ ( if null subtrees
                              then empty
                              else text "subderivations"
                                   $+$ nest 2 ( vcat subTreeDocs )
                            )
                  )

    createDoc (k,v) = let nm = show k
                          size = defDocWidth - 4 * level - length nm - 3
                      in do (t,_) <- tablify (SamePrioNo 9) size v
                            return (renderTableToDoc t)

    mkLines s = let size = defDocWidth - 4 * level
                    s'   = wrapString size s
                in vcat $ map text s'
toDocTT _ name (DTLeaf _ val)
  = return (text (show name) <+> text "value" <+> text (show val))

treeToStringTT :: Monad m => DerivationTree -> TTM m String
treeToStringTT root
  = do (rDoc, refDocs, _) <- processReferences (toDocTT 1 (ident "root") root) id guessToDoc
       return $ render (rDoc $+$ ( vcat [ text (show g) <> text ":" <+> d | (g,d) <- refDocs ] ))
  where
    guessToDoc g
      = do mb <- valLookupIT g
           (t,_) <- case mb of
                      Just v ->  tablify (SamePrioNo 9) (defDocWidth - 6) v
                      Nothing -> tablifyGuess g
           return (renderTableToDoc t)


-- Expands all values in the tree to the maximal extend possible
expandTree :: Monad m => DerivationTree -> IT m DerivationTree
expandTree t@(DTNode _ _ _ _ _ _ _)
  = do inputs'  <- attrsExpand (dtInputs t)
       outputs' <- attrsExpand (dtOutputs t)
       visits'  <- mapM visitExpand (dtVisits t)
       return (t { dtInputs = inputs', dtOutputs = outputs', dtVisits = visits' })
expandTree (DTLeaf g v)
  = do v' <- expAll valLookupIT v
       return (DTLeaf g v')

visitExpand :: Monad m => DerivationVisit -> IT m DerivationVisit
visitExpand (DTVisit nm subtrees messages)
  = do subtrees' <- mapM (\(k,g,t) -> expandTree t >>= \t' -> return (k,g,t')) subtrees
       return (DTVisit nm subtrees' messages)

-- Expands the value to the maximal extend possible
attrsExpand :: Monad m => [(Ident,Value)] -> IT m [(Ident,Value)]
attrsExpand
  = mapM (\(i,v) -> expAll valLookupIT v >>= \v' -> return (i,v'))


--
-- Externals
--

newtype SingleRes a = SingleRes a
newtype DoubleRes a b = DoubleRes (a,b)
newtype InOnly a = InOnly a             -- a value that is only going in the function
newtype Fixed a = Fixed a               -- a non-guess value that is going in the function
newtype FixedInOnly a = FixedInOnly a   -- a non-guess value that is only going in the function

class RulerValueOrPoly a where
  mkWrapper :: a -> I Value
  rmWrapper :: Value -> I a

instance RulerValueOrPoly Poly where
  mkWrapper = createPolyWrapper
  rmWrapper = removeWrapperPoly

instance RulerValueOrPoly (PolyArg a) where
  mkWrapper (PolyArg p) = createPolyWrapper p
  rmWrapper v = removeWrapperPoly v >>= return . PolyArg

instance RulerValue a => RulerValueOrPoly a where
  mkWrapper = createWrapper
  rmWrapper = removeWrapper


data TupExtract = forall r . IsTuple r => Head (I Value) r | TupEnd

class IsTuple t where
  extractOne :: t -> TupExtract
  components :: t -> Int

instance IsTuple TupExtract where
  extractOne = id
  components (Head _ r) = 1 + components r
  components TupEnd     = 0

instance (RulerValueOrPoly a, IsTuple r) => IsTuple (a, r) where
  extractOne (a,r) = Head (mkWrapper a) r
  components ~(_,r) = 1 + components r

instance RulerValueOrPoly a => IsTuple (SingleRes a) where
  extractOne (SingleRes a) = Head (mkWrapper a) TupEnd
  components _ = 1

instance (RulerValueOrPoly a, RulerValueOrPoly b) => IsTuple (DoubleRes a b) where
  extractOne (DoubleRes (a,b)) = Head (mkWrapper a) $ Head (mkWrapper b) TupEnd
  components _ = 2

instance (RulerValueOrPoly a, RulerValueOrPoly b, RulerValueOrPoly c) => IsTuple (a, b, c) where
  extractOne (a, b, c) = Head (mkWrapper a) $ Head (mkWrapper b) $ Head (mkWrapper c) TupEnd
  components _ = 3

instance IsTuple () where
  extractOne _ = TupEnd
  components _ = 0

tupToList :: IsTuple r => r -> I [Value]
tupToList t = case extractOne t of
                Head m r -> do v <- m
                               vs <- tupToList r
                               return (v : vs)
                TupEnd   -> return []

data AppStep = forall r . IsExternal r => TookOneArg (I r)

class IsExternal f where
  appOneArg   :: f -> Value -> AppStep
  appFinished :: f -> I [Value]
  arguments   :: f -> Int
  results     :: f -> Int

instance (RulerValueOrPoly a, IsExternal r) => IsExternal (a -> r) where
  appOneArg f v = TookOneArg $ do a <- rmWrapper v
                                  return (f a)
  appFinished _ = error "appFinished: function takes still some arguments."
  arguments f   = 1 + arguments (f (error "arguments: too much evaluation"))
  results   f   = results (f (error "results: too much evaluation"))

instance (RulerValue a, IsExternal r) => IsExternal (InOnly a -> r) where
  appOneArg f v = TookOneArg $ do a <- removeWrapperInOnly v
                                  return (f a)
  appFinished _ = error "appFinished: function takes still some arguments."
  arguments f   = 1 + arguments (f (error "arguments: too much evaluation"))
  results   f   = results (f (error "results: too much evaluation"))

instance (RulerValue a, IsExternal r) => IsExternal (Fixed a -> r) where
  appOneArg f v = TookOneArg $ do a <- removeWrapperFixed v
                                  return (f a)
  appFinished _ = error "appFinished: function takes still some arguments."
  arguments f   = 1 + arguments (f (error "arguments: too much evaluation"))
  results   f   = results (f (error "results: too much evaluation"))

instance (RulerValue a, IsExternal r) => IsExternal (FixedInOnly a -> r) where
  appOneArg f v = TookOneArg $ do a <- removeWrapperFixedInOnly v
                                  return (f a)
  appFinished _ = error "appFinished: function takes still some arguments."
  arguments f   = 1 + arguments (f (error "arguments: too much evaluation"))
  results   f   = results (f (error "results: too much evaluation"))

instance IsTuple t => IsExternal (I t) where
  appOneArg _ _ = error "appOneArg: function is not expecting any arguments anymore."
  appFinished m = do t <- m
                     tupToList t
  arguments _   = 0
  results f = let z :: I t -> t
                  z _ = error "results: too much evaluation"
              in components (z f)

removeWrapper :: forall a . RulerValue a => Value -> I a
removeWrapper (ValueOpaque _ a _)
  = case cast a of
      Just r  -> return r
      Nothing -> abort ("failed to remove wrapper around " ++ show a ++ ", contained value has type: " ++ show (typeOf a) ++ ", while expecting: " ++ show (typeOf (undefined :: a)))
removeWrapper (ValueGuess g) = toIndirection (IndInfo g) Nothing
removeWrapper v              = abort ("not an opaque value nor a guess: " ++ show v)

removeWrapperInOnly :: RulerValue a => Value -> I (InOnly a)
removeWrapperInOnly v
  = do a <- removeWrapper v
       a1 <- extExpand a
       case fromIndirection a1 of
         (_, Just a2) -> return (InOnly a2)
         _            -> return (InOnly a1)

removeWrapperFixed :: RulerValue a => Value -> I (Fixed a)
removeWrapperFixed v
  = do a <- removeWrapper v
       a1 <- extExpand a
       case fromIndirection a1 of
         (_, Nothing) -> abort ("expecting a concrete value, instead of guess " ++ show v)
         _            -> return (Fixed a1)

removeWrapperFixedInOnly :: RulerValue a => Value -> I (FixedInOnly a)
removeWrapperFixedInOnly v
  = do a <- removeWrapper v
       a1 <- extExpand a
       case fromIndirection a1 of
         (_, Nothing) -> abort ("expecting a concrete value, instead of guess " ++ show v)
         (_, Just a2) -> return (FixedInOnly a2)

removeWrapperPoly :: Value -> I Poly
removeWrapperPoly v
  = do v' <- expand v
       case v' of
         ValueOpaque g a _ -> do a' <- toIndirection (IndInfo g) (Just a)
                                 return (Poly a)
         _                 -> return (Poly v')  -- polymorphism trick

createWrapper :: RulerValue a => a -> I Value
createWrapper a
  = case fromIndirection a of
      (Just (IndInfo g), _) -> return $ ValueGuess g
      (Nothing, _)          -> mkOpaque a

createPolyWrapper :: Poly -> I Value
createPolyWrapper (Poly a) = createWrapper a

appExternal :: IsExternal f => f -> [Value] -> I [Value]
appExternal f = run . foldl next init
  where
    run m = do (TookOneArg r) <- m
               f <- r
               appFinished f
    init = return $ TookOneArg $ return f
    next rec v
      = do (TookOneArg r) <- rec
           f <- r
           return $ appOneArg f v

data External = forall f . IsExternal f => External f Bool

mkExt :: IsExternal f => Bool -> String -> f -> (Ident, External)
mkExt isVisible n f = (ident n, External f isVisible)

mkExtData :: IsExternal f => String -> f -> (Ident, External)
mkExtData = mkExt False

mkResult1 :: (Show a, Typeable a, RulerValue a, Tabular a) => a -> I (Value, ())
mkResult1 x
  = do v' <- mkOpaque x
       return (v', ())

mk1ResultTup :: RulerValue a => a -> I (a, ())
mk1ResultTup x = return (x, ())

extVoidResult :: I ()
extVoidResult = return ()

extSingleResult :: RulerValueOrPoly a => a -> I (SingleRes a)
extSingleResult x = return (SingleRes x)

extDoubleResult :: (RulerValue a, RulerValue b) => a -> b -> I (DoubleRes a b)
extDoubleResult x y = return (DoubleRes (x, y))

extTrippleResult :: (RulerValue a, RulerValue b, RulerValue c) => a -> b -> c -> I (a, b, c)
extTrippleResult x y z = return (x, y, z)


{-
data AppStep' = forall r a . IsExternal' r a => TookOneArg' (I r)

class IsExternal f => IsExternal' f a | f -> a where
  appOneArg'   :: f -> a -> AppStep'
  appFinished' :: f -> I a

instance (RulerValueOrPoly a, RulerValueOrPoly b, IsExternal' r b) => IsExternal' (a -> r) a where
  appOneArg' f a = TookOneArg' $ do v <- mkWrapper a
                                    b <- rmWrapper v
                                    return (f b)
  appFinished' _ = error "external function still accepts some more arguments"

instance (IsExternal t, IsTuple' t a) => IsExternal' t a where
  appOneArg' _ _ = error "external function does not accept more arguments"
  appFinished' m = do t <- m
                      tupToList' t

data TupExtract' a = forall r b . IsTuple' r b => Head' (I a) r | TupEnd'

class IsTuple t => IsTuple' t a | t -> a where
  extractOne' :: t -> TupExtract' a

instance RulerValueOrPoly a => IsTuple' (TupExtract' a) a where
  extractOne' = id

instance RulerValueOrPoly a => IsTuple (TupExtract' a) where
  extractOne (Head' m r) = Head (m >>= mkWrapper) r
  extractOne TupEnd'     = TupEnd
  components (Head' _ r) = 1 + components r
  components TupEnd'     = 0

instance (RulerValueOrPoly a, RulerValueOrPoly b, IsTuple' r b) => IsTuple' (a, r) a where
  extractOne' (a,r) = Head' (mkWrapper a >>= rmWrapper) r

instance RulerValueOrPoly a => IsTuple' (SingleRes a) a where
  extractOne' (SingleRes a) = Head' (mkWrapper a >>= rmWrapper) (TupEnd' :: TupExtract' ())

instance IsTuple' () a where
  extractOne' _ = TupEnd'
-}


--
-- List of labels
--

showLblsShort :: Set Pos -> String
showLblsShort ps
  = intercalate "-" (map (\p -> show (line p) ++ "," ++ show (column p)) (Set.toList ps))


--
-- Render a Table to a Doc
--

renderTableToDoc :: Table -> Doc
renderTableToDoc t
  = case t of
      TableEmpty       -> empty
      TableText s      -> text s
      TableRef _ _ t   -> renderTableToDoc t
      TableBesides l r -> renderTableToDoc l <> renderTableToDoc r
      TableAbove u l   -> renderTableToDoc u $+$ renderTableToDoc l


--
-- DOT (graphviz) generation
--

type TTMD a = TTM Dot a

treeToDotStringTT :: Monad m => DerivationTree -> TTM m String
treeToDotStringTT root
  = do opts <- asks itOpts
       let dir = if dirBT opts then "BT" else "TB"
           header = "graph [fontsize=\"12\" fontname=\"Courier\" labelloc=\"t\" splines=\"true\" overlap=\"false\" rankdir=\"" ++ dir ++ "\" ];"
       switchStateIT (mapStateT (render header)) genGraph
  where
    render h m = let (str,(_,s)) = showDot h m
                 in return (str,s)
    genGraph = do (treeRefs, valRefs, gsInit) <- processReferences (wrap $ toDotTT (ident "root") root) wrap guessToDotTT
                  let refs = Set.toList treeRefs
                             ++ [ (g,p,g') | (g,ps) <- valRefs, (p,g') <- Set.toList ps ]
                  mapM_ (mkValArrow gsInit) refs
    wrap = switchStateIT $ mapStateT $ mkCluster
    mkCluster m = do (_,a) <- cluster $ do attribute ("style", "invis")
                                           m
                     return a


toDotTT :: Ident -> DerivationTree -> TTMD (Set (Guess, Int, Guess))
toDotTT name (DTNode uid title context inputs outputs visits status)
  = do opts <- asks itOpts
       let treesBottomToTop = dirBT opts
       inps <- mapM (mkAttr inpAttr) inputs
       outs <- mapM (mkAttr outAttr) outputs
       let subtrees = [ (t,vNm) | (DTVisit vNm subtrees _) <- visits, t <- subtrees ]
       let messages = [ (ms,vNm) | (DTVisit vNm _ ms) <- visits, not (null ms) ]
       subs <- mapM (\((nm,_,t),_) -> toDotTT nm t) subtrees
       if length subtrees > 1
        then runDot $ same (map (\((_,g,_),_) -> userNodeId g) subtrees)
        else return ()
       let tbl  = H.table (H.concatHtml $ map H.tr rows) H.! (tblExtra ++ [ H.border 1, H.intAttr "cellborder" 0, H.cellspacing 2, H.cellpadding 0, H.bgcolor "white" ])
           rows =  (if treesBottomToTop then subtreerows else [])
                ++ [ H.td (identHtmlW title +++ grayFont (context)) H.! [ H.bgcolor "black", H.align "center" ] ]
                ++ subtitle
                ++ [ H.td $ smalltable $ H.concatHtml $ map fst (inps ++ outs) ]
                ++ (if not $ null messages then map (\(ms,vNm) -> H.td $ smalltable $ H.concatHtml $ ((if length messages <= 1 then id else (msgTitle (show vNm) :)) (zipWith mkMsg [1..] ms))) messages else [])
                ++ (if treesBottomToTop then [] else subtreerows)
           subtreerows = (if not $ null subtrees then [ H.td (smalltable $ H.tr $ H.concatHtml (map mkChildRef subtrees)) H.! [ H.align "center", H.bgcolor "lightsteelblue2" ] ] else [])
           refs = Set.fromList $ concatMap (\(_,r) -> [ (uid,p,g') | (p,g') <- Set.toList r ]) (inps ++ outs)
           extra | show name == "root" = [("root","true")]
                 | otherwise           = []
       htmlNode extra uid tbl
       mapM_ mkChildArr subtrees
       return (Set.unions (refs : subs))
  where
    mkAttr mode (i,v)
      = do (h, refs, s) <- valueToHtml 50 v
           return ( H.tr ( H.td (whiteFont mode) H.! [ H.align "center", H.bgcolor "gray25" ]
                         +++ H.td (blackFont $ " " ++ show i ++ " ") H.! [ H.align "left", H.bgcolor "gray93" ]
                         +++ H.td (smalltable $ H.tr $ (H.td h H.! [ H.align "left" ])) H.! [ H.align "left" ] )
                  , refs)
    inpAttr = " I "
    outAttr = " O "
    mkMsg n (NotifyMsg s) = let ss = wrapString 60 s
                            in H.tr ((H.td (blackFont (show n ++ ": ")) H.! [ H.bgcolor "gray88", H.align "right", H.strAttr "VALIGN" "TOP" ]) +++ (H.td (smalltable $ H.concatHtml $ map (\s' -> H.tr $ ((H.td $ blackFont s') H.! [ H.align "left" ])) ss) H.! [ H.align "left", H.bgcolor "gray95" ]))
    mkMsg n (ErrorMsg s)  = let ss = wrapString 60 s
                            in H.tr ((H.td (redFont (show n ++ "! ")) H.! [ H.bgcolor "gray88", H.align "right", H.strAttr "VALIGN" "TOP" ]) +++ (H.td (smalltable $ H.concatHtml $ map (\s' -> H.tr $ ((H.td $ redFont s') H.! [ H.align "left" ])) ss) H.! [ H.align "left", H.bgcolor "gray95" ]))
    msgTitle s = H.tr (((H.td (blackFont "")) H.! [ H.bgcolor "gray88", H.align "right" ]) +++ ((H.td (blackFont ("visit: " ++ s))) H.! [ H.bgcolor "gray95", H.align "left" ]) )
    mkChildRef ((nm,g,_),vNm) = H.td (identHtmlB' nm) H.! [ H.align "center", H.strAttr "PORT" ("c" ++ show g) ]
    mkChildArr ((_,g,_),_)    = runDot $ edge' (userNodeId uid) (Just ("c" ++ show g)) (userNodeId g) Nothing [("arrowhead", "dot"),("dir","back"),("style","bold")]
    
    tblExtra = case status of
                 Success   -> [ H.color "black" ]
                 Failure _ -> [ H.color "crimson" ]
    subtitle = case status of
                 Success   -> []
                 Failure s -> [ H.td ( smalltable $ H.concatHtml $ map (\s' -> H.tr (H.td (whiteFont s') H.! [ H.align "left" ])) (wrapString 60 s) ) H.! [ H.bgcolor "crimson", H.align "left" ] ]
toDotTT _ (DTLeaf uid v)
  = do (h,info,_) <- valueToHtml 80 v
       htmlNode [] uid h
       return (Set.map (\(p,g') -> (uid,p,g')) info)

identHtmlB = identHtml blackFont goldFont
identHtmlB' = identHtml blackFont whiteFont
identHtmlW = identHtml whiteFont goldFont

identHtml :: (String -> Html) -> (String -> Html) -> Ident -> Html
identHtml f g nm = f (show nm)
                   H.+++ ( if line p <= 0
                           then H.noHtml
                           else g (" (" ++ l ++ "," ++ c ++ ")") )
  where
    p = identPos nm
    l = show (line p)
    c = show (column p)

identHtmlAttr :: Ident -> Html
identHtmlAttr = identHtmlB

whiteFont = colored "white"
grayFont  = colored "gray73"
blackFont = colored "black"
goldFont  = colored "gold"
blueFont  = colored "dodgerblue3"
redFont   = colored "crimson"

colored :: String -> String -> Html
colored c s
  | null s    = H.noHtml
  | otherwise = H.font (H.primHtml $ escape $ H.stringToHtmlString s) H.! [ H.color c ]
  where escape [] = []
        escape (c:cs)
          | c == '['  = "&#91;" ++ escape cs
          | c == ']'  = "&#93;" ++ escape cs
          | otherwise = c : escape cs

htmlNode :: [(String,String)] -> Guess -> Html -> TTMD ()
htmlNode extra g html
  = runDot $ userNode (userNodeId g) (extra ++ [("label", s),("style","filled"), ("penwidth","1"),("fillcolor","white"), ("shape","plaintext"),("fontname","Courier"),("fontsize","12")])
  where s = "<" ++ foldr (.) id (map (H.renderHtml' 0) (H.getHtmlElements html)) ">"

runDot :: Dot a -> TTMD a
runDot = lift . lift

guessToDotTT :: Guess -> TTMD (Set (Int, Guess))
guessToDotTT g
  = do mb <- valLookupIT g
       case mb of
         Just v -> do (h, info,_) <- valueToHtml 30 v
                      htmlNode [] g (H.table (H.tr (H.td (whiteFont ("#" ++ show g)) H.! [ H.align "center", H.bgcolor "black" ] ) +++ H.tr (H.td h H.! [ H.align "left" ])) H.! [ H.bgcolor "white", H.border 1, H.cellspacing 2 ])
                      return info
         Nothing -> return Set.empty

mkValArrow :: IntSet -> (Guess, Int, Guess) -> TTMD ()
mkValArrow gsInit (srcNode, locId, dstNode)
  = runDot $ edge' (userNodeId srcNode) (Just ("p" ++ show locId)) (userNodeId dstNode) Nothing (extra ++ [("color","gray60"),("style","dotted")])
  where
    extra | dstNode `IntSet.member` gsInit = [("contraint", "false")]
          | otherwise                      = []

valueToHtml :: Int -> Value -> TTMD (Html, Set (Int, Guess), Int)
valueToHtml size v
  = do (t,_) <- tablify (SamePrioNo 9) size v
       let ((h,s), gs) = runWriter $ renderTableToHtml t
       return (h,gs,s) 

renderTableToHtml :: Table -> Writer (Set (Int, Guess)) (Html, Int)
renderTableToHtml t
  = case t of
      TableEmpty     -> return (H.noHtml, 0)
      TableText s    -> return (blackFont s, length s)
      TableRef p g t -> do tell (Set.singleton (p, g))
                           (h, s) <- renderTableToHtml t
                           return (smalltable $ H.tr $ (smalltd h H.! [ H.strAttr "port" ("p" ++ show p) ]), s)
      TableBesides l r -> do (hl,s1) <- renderTableToHtml l
                             (hr,s2) <- renderTableToHtml r
                             let s = s1 + s2
                             return (smalltable $ H.tr (smalltd hl +++ smalltd hr), s)
      TableAbove u b   -> do (hu,s1) <- renderTableToHtml u
                             (hb,s2) <- renderTableToHtml b
                             let s = max s1 s2
                             return (smalltable ((H.tr $ smalltd hu) +++ (H.tr $ smalltd $ hb)), s)

smalltable :: Html -> Html
smalltable h = H.table h H.! [ H.border 0, H.cellspacing 0, H.cellpadding 0, H.align "left" ]

smalltd :: Html -> Html
smalltd h = H.td h H.! [ H.align "left" ]

width :: Int -> String
width s = show (s * 10 + 2)

defHeight :: Int
defHeight = 18


--
-- Create a total ordering out of relative orders
--

totalOrder :: [[Ident]] -> [Ident]
totalOrder relOrders
  = G.flattenSCCs cmps
  where
    allIdents = nub $ concat relOrders

    allPreds i = concatMap (preds i) relOrders

    preds i relOrder
      | i `elem` relOrder = takeWhile (/= i) relOrder
      | otherwise         = []

    cmps = G.stronglyConnComp [ (i,i,allPreds i) | i <- allIdents ]


-- LaTeX TT monad version
-- For now, values are printed verbatim, no escaping performed.
toTexDocTT :: Monad m => DerivationTree -> TTM m Doc
toTexDocTT (DTNode _ title context inputs outputs visits status)
  = do subtreeDocs <- mapM toTexDocTT [ t | (DTVisit _ subtrees _) <- visits, (_,_,t) <- subtrees ]
       inputDocs   <- mapM createAttrDoc inputs
       outputDocs  <- mapM createAttrDoc outputs
       
       let mkLabel = case status of
                       Success   -> text (show title)
                       Failure _ -> text ("{" ++ show title ++ "}")
       
       return (   (text "\\inferrule*[right=" <> mkLabel <> text "]")
              $+$ (nest 2 $ braces $ vcat [ doc <+> text "\\\\" | doc <- subtreeDocs ])
              $+$ (nest 2 $ braces ( mkProd inputDocs <+> text "\\rightarrow" <+> mkProd outputDocs ))   -- here we want a custom pp
              )
  where
    mkProd xs = text "\\mathsf{" <> (parens $ hsep $ punctuate (text ",") xs) <> text "}"
  
    createAttrDoc (_,v) = do (t,_) <- tablify (SamePrioNo 9) defDocWidth v
                             return (renderTableToDoc t)
toTexDocTT (DTLeaf _ val)
  = return $ text ("\\mbox{" ++ show val ++ "}")

treeToTexStringTT :: Monad m => Bool -> DerivationTree -> TTM m String
treeToTexStringTT ruleOnly root
  = do (rDoc, refDocs, _) <- processReferences (toTexDocTT root) id guessToDoc
       let rDoc' = text "\\def\\rulerrules" <> braces rDoc
           refDoc = text "\\def\\rulerrefs" <> braces refTable
           refTable = text "\\begin{tabular}{l|l}\\hline"
                      $+$ ( vcat [ text (show g) <+> text "& \\ensuremath{\\mathsf{" <+> d <+> text "}}\\\\\\hline" | (g,d) <- refDocs ] )
                      $+$ text "\\end{tabular}"
       return $ render $
         if ruleOnly
         then rDoc'
         else rDoc' $+$ refDoc
  where
    guessToDoc g
      = do mb <- valLookupIT g
           (t,_) <- case mb of
                      Just v ->  tablify (SamePrioNo 9) (defDocWidth - 6) v
                      Nothing -> tablifyGuess g
           return (renderTableToDoc t)

