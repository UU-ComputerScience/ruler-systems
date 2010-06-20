module Opts where

import System.Console.GetOpt
import System.Environment
import System.FilePath
import System.IO
import System.Directory

data Opts
  = Opts { sourceFile :: !FilePath
         , outputFile :: !(Maybe FilePath)
         , verbose :: !Bool
         , tokens :: !Bool
         , pretty :: !Bool
         , genHaskell :: !Bool
         , forceGen :: !Bool
         }

opts :: [OptDescr (Opts -> IO Opts)]
opts = [ Option "o" ["output"] (ReqArg oOutput "path") "output .hs file"
       , Option "v" ["verbose"] (NoArg oVerbose) "verbose output"
       , Option ""  ["pretty"] (NoArg oPretty) "pp AST to STDOUT"
       , Option ""  ["tokens"] (NoArg oTokens) "print tokens to STDOUT"
       , Option ""  ["haskell"] (NoArg oHaskell) "generate Haskell code (default)"
       , Option "f" ["force"] (NoArg oForce) "force code generation"
       ]

oOutput :: FilePath -> Opts -> IO Opts
oOutput s o = return (o { outputFile = Just s })

oVerbose :: Opts -> IO Opts
oVerbose o = return (o { verbose = True })

oPretty :: Opts -> IO Opts
oPretty o = return (o { pretty = True })

oTokens :: Opts -> IO Opts
oTokens o = return (o { tokens = True })

oHaskell :: Opts -> IO Opts
oHaskell o = return (o { genHaskell = True })

oForce :: Opts -> IO Opts
oForce o = return (o { forceGen = True })

defaultOpts :: Opts
defaultOpts = Opts { sourceFile = "", outputFile = Nothing, verbose = False, pretty = False, tokens = False
                   , genHaskell = True, forceGen = False }

commandlineArgs :: IO Opts
commandlineArgs
  = do args <- getArgs
       let usage = usageInfo "Usage: imp-visits <OPTION ...> <ag-file> ..." opts
       case getOpt Permute opts args of
         (actions, args', []) | null args' -> do hPutStrLn stderr ("No AG source file specified.\n" ++ usage)
                                                 foldl (>>=) (return defaultOpts) actions
                              | otherwise  -> foldl (>>=) (return $ patch $ defaultOpts { sourceFile = head args' }) actions
         (_, _, errs)                      -> do hPutStrLn stderr (unlines errs ++ "\n" ++ usage)
                                                 return defaultOpts
  where
    patch o = if sourceFile o /= ""
              then o { outputFile = Just $ replaceExtension (sourceFile o) ".hs" }
              else o
