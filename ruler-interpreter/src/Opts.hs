module Opts where


import System.Console.GetOpt
import System.Environment


data Opts
  = Opts { sourceFile   :: !FilePath
         , outputFile   :: !(Maybe FilePath)
         , extraFiles   :: ![FilePath]
         , targetFormat :: !Format
         , expansion    :: !ExpansionMode
         , dirBT        :: !Bool
         , debugLevel   :: !Int
         , dumpMessages :: !Bool
         , programArgs  :: ![String]
         }

data Format = Text | Dot | Tex | None deriving Eq
data ExpansionMode = ExpandNone | ExpandSome | ExpandFull deriving Eq

opts :: [OptDescr (Opts -> Opts)]
opts = [ Option "o" ["output"]   (OptArg oOutput "path")           "output .dot file"
       , Option "T" ["target"]   (ReqArg oTarget "txt|dot|none")   "output format of derivation"
       , Option "d" ["debug"]    (OptArg oDebug "level")           "debug level"
       , Option "m" ["messages"] (NoArg oMessages)                 "output messages to stderr"
       , Option "I" ["include"]  (ReqArg oInclude "path")          "include statements"
       , Option "E" ["expand"]   (ReqArg oExpand "none|some|full") "amount of attribute expansion during pretty"
       , Option "D" ["dir"]      (ReqArg oDir "BT|TB")             "grow direction of the derivation tree"
       ]

oOutput :: Maybe FilePath -> Opts -> Opts
oOutput s o = o { outputFile = s }

oTarget :: String -> Opts -> Opts
oTarget s o | s == "dot" = o { targetFormat = Dot  }
            | s == "txt" = o { targetFormat = Text }
            | s == "tex" = o { targetFormat = Tex  }
            | otherwise  = o

oDir :: String -> Opts -> Opts
oDir s o | s == "BT" = o { dirBT = True  }
         | s == "TB" = o { dirBT = False }
         | otherwise = o

oDebug :: Maybe String -> Opts -> Opts
oDebug Nothing  o = o { debugLevel = 1 }
oDebug (Just s) o = o { debugLevel = read s }

oMessages :: Opts -> Opts
oMessages o = o { dumpMessages = True }

oInclude :: String -> Opts -> Opts
oInclude s o = o { extraFiles = extraFiles o ++ [s] }

oExpand :: String -> Opts -> Opts
oExpand s o | s == "none" = o { expansion = ExpandNone }
            | s == "some" = o { expansion = ExpandSome }
            | otherwise   = o { expansion = ExpandFull }

defaultOpts :: Opts
defaultOpts = Opts { sourceFile   = ""
                   , outputFile   = Nothing
                   , extraFiles   = []
                   , targetFormat = Text
                   , dirBT        = True
                   , expansion    = ExpandSome
                   , debugLevel   = 0
                   , dumpMessages = False
                   , programArgs  = []
                   }

commandlineArgs :: IO Opts
commandlineArgs
  = do args <- getArgs
       case getOpt Permute opts args of
         (os, args', []) | null args' -> ioError (userError ("No ruler source file specified.\n" ++ usageInfo "Usage: ruler <OPTION ...> <ruler file> ..." opts))
                         | otherwise  -> return $ (foldr ($) defaultOpts os) { sourceFile = head args', programArgs = tail args' }
         (_, _, errs)                 -> ioError (userError (unlines errs ++ "\n" ++ usageInfo "Usage: ruler <OPTION ...> <ruler file> ..." opts))

