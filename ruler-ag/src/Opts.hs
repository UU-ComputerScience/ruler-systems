module Opts where

import System.Console.GetOpt
import System.Environment
import System.FilePath

data Opts
  = Opts { sourceFile :: !FilePath
         , outputFile :: !FilePath
         }

opts :: [OptDescr (Opts -> IO Opts)]
opts = [ Option "o" ["output"] (ReqArg oOutput "path") "output .hs file"
       ]

oOutput :: FilePath -> Opts -> IO Opts
oOutput s o = return (o { outputFile = s }) 

defaultOpts :: Opts
defaultOpts = Opts { sourceFile = "", outputFile = "" }

commandlineArgs :: IO Opts
commandlineArgs
  = do args <- getArgs
       let usage = usageInfo "Usage: ruler-ag <OPTION ...> <ag-file> ..." opts
       case getOpt Permute opts args of
         (actions, args', []) | null args' -> ioError (userError ("No AG source file specified.\n" ++ usage))
                              | otherwise  -> foldl (>>=) (return $ patch $ defaultOpts { sourceFile = head args' }) actions
         (_, _, errs)                      -> ioError (userError (unlines errs ++ "\n" ++ usage))
  where
    patch o = o { outputFile = replaceExtension (sourceFile o) ".hs" }

