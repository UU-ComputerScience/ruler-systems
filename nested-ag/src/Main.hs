module Main where

import Scanner
import Parser
import BackendAst
import Transform
import Opts
import Control.Monad.Error
import UU.Scanner.Position

main :: IO ()
main = do opts  <- commandlineArgs
          let path = sourceFile opts
          str <- readFile path
          let tks = tokenize path str
              pres = parseAg opts (Pos 1 1 path) tks
          -- putStrLn (show $ ppTokens tks)
          case pres of
            Left err  -> putStrLn err
            Right ast -> writeFile (outputFile opts) (transform ast)
