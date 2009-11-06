module Main where

import Scanner
import Parser
import BackendAst
import Transform
import Opts
import Control.Monad.Error

parseFile :: FilePath -> IO (Either String Ag)
parseFile path
  = do str <- readFile path
       return (parse str)
  where
    parse str
      = tokenize path str >>= parseTokens pAg

main :: IO ()
main = do opts  <- commandlineArgs
          agAst <- parseFile (sourceFile opts)
          case agAst of
            Left err  -> putStrLn err
            Right ast -> writeFile (outputFile opts) (transform ast)

