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
              pos  = Pos 1 1 path
          when (path /= "") $
            do str <- readFile path
               let tks  = tokenize path str
                   pres = parseProgram opts pos tks
               when (tokens opts) (putStrLn (show $ ppTokens tks))
               case pres of
                 Left err  -> do hPutStrLn stderr err
                                 exitFailure
                 Right ast -> let (errs, txtId, txtTarget, txtGraph) = transform pos opts ast
                              in do when (pretty opts) (putStrLn txtId)
                                    when (not $ nullErrs errs) $
                                      hPutStrLn stderr (errsToStr opts errs)
                                    case outputGraph opts of
                                      Just name -> writeFile name txtGraph
                                      Nothing   -> return ()
                                    if nullErrs errs || forceGen opts
                                      then case outputFile opts of
                                             Just name -> writeFile name txtTarget
                                             Nothing   -> return ()
                                      else exitFailure
