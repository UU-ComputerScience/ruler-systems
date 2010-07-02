module Main where

import Scanner
import Parser
import Opts
import Common
import Transform
import Errs
import Control.Monad
import System
import System.IO

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
                                 hFlush stderr
                                 exitFailure
                 Right ast -> let (errs, txtId, txtTarget, txtGraph) = transform pos opts ast
                              in do when (pretty opts) (putStrLn txtId)
                                    when (not $ nullErrs errs) $ do
                                      hPutStrLn stderr (errsToStr opts errs)
                                      hFlush stderr
                                    case outputGraph opts of
                                      Just name -> myWriteFile name txtGraph
                                      Nothing   -> return ()
                                    if nullErrs errs || forceGen opts
                                      then case outputFile opts of
                                             Just name -> myWriteFile name txtTarget
                                             Nothing   -> return ()
                                      else exitFailure

myWriteFile :: FilePath -> String -> IO ()
myWriteFile path txt
  = seq (seqChars txt) $ withFile path WriteMode writeHandle
  where
    seqChars []     = ()
    seqChars (c:cs) = seq c (seqChars cs)

    writeHandle h = do hPutStrLn h txt
                       hFlush h
