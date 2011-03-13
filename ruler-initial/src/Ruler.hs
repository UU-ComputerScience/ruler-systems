module Main(main) where

import System.Console.GetOpt(usageInfo)
import System.Directory(doesFileExist)
import System.Environment(getArgs)
import System.Exit(exitFailure)

import Util.Options
import Util.Error.Err
import Util.Error.Pretty

import Core.Core
import Core.Pretty

import qualified Base.Main as T1


main :: IO ()
main
  = do args <- getArgs
       let (opts, sources, optsErrs) = getOptions args
       if null optsErrs && length sources == 1
        then do let file = head sources
                exists <- doesFileExist file
                if exists
                 then do res <- T1.compile (head sources)
                         case res of
                           Left errs  -> do mapM_ (putStrLn . prettyErr (hasVerboseFlag opts)) errs
                                            exitFailure
                           Right cMod -> putStrLn (pretty cMod)
                 else do putStrLn (file ++ ": this file does not exist")
                         exitFailure
        else do mapM_ putStrLn (usageInfo "Usage info: ruler options source-file\n" options : optsErrs)
                exitFailure

