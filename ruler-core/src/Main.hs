module Main where

import Scanner
import Parser
import Opts
import Common
import PrettyAst
import Transform
import Errs
import Control.Monad
import System.IO

main :: IO ()
main = do opts  <- commandlineArgs
          let path = sourceFile opts
          when (path /= "") $
            do str <- readFile path
               let tks  = tokenize path str
                   pres = parseProgram opts (Pos 1 1 path) tks
               when (tokens opts) $
                 putStrLn (show $ ppTokens tks)
               case pres of
                 Left err  -> putStrLn err
                 Right ast -> do when (pretty opts) $
                                   putStrLn (progToStr ast)
                                 case outputFile opts of
                                   Nothing   -> return ()
                                   Just file -> let errs = transform ast
                                                in hPutStrLn stderr (errsToStr opts errs)
                                                   -- writeFile (outputFile opts) (transform ast)
