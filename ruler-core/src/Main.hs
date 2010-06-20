module Main where

import Scanner
import Parser
import Opts
import Common
import Transform
import Errs
import Control.Monad
import System.IO
import System

main :: IO ()
main = do opts  <- commandlineArgs
          let path = sourceFile opts
          when (path /= "") $
            do str <- readFile path
               let tks  = tokenize path str
                   pres = parseProgram opts (Pos 1 1 path) tks
               when (tokens opts) (putStrLn (show $ ppTokens tks))
               case pres of
                 Left err  -> do hPutStrLn stderr err
                                 exitFailure
                 Right ast -> let (errs, txtId, txtTarget) = transform opts ast
                              in do when (pretty opts) (putStrLn txtId)
                                    when (not $ nullErrs errs) $
                                      hPutStrLn stderr (errsToStr opts errs)
                                    if nullErrs errs || forceGen opts
                                      then case outputFile opts of
                                             Just name -> writeFile name txtTarget
                                             Nothing   -> return ()
                                      else exitFailure
