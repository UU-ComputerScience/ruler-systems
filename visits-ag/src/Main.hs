module Main where

import Scanner
import Parser
import System
import System.Exit
import System.IO
import Flatten
import Desugar
import UU.Scanner.Position
import Merger
import Opts
import qualified Data.Sequence as Seq
import Data.Sequence(Seq)
import UU.Pretty
import Error
import PrettyFront
import PrettyFlat
import PrettySimple

main :: IO ()
main = do args <- getArgs
          opts <- step PrettyNone parseOpts args
          let filename = optSource opts
          str <- readFile filename
          let tks = tokenize filename str
          case optDumpTks opts of
            Just mbPath -> dump ppTokens mbPath tks
            _           -> return ()
          vast <- step (mbPretty ppVag (optDumpFront opts)) (parseTokens opts (Pos 1 1 filename) pVag) tks
          fast <- step (mbPretty ppFag (optDumpFlat opts)) flatten vast
          dast <- step (mbPretty ppSag (optDumpDesugared opts)) desugar fast
          return ()

data PrettyOpt a
  = PrettyNone
  | PrettyYes (a -> PP_Doc) (Maybe FilePath)

mbPretty :: (a -> PP_Doc) -> (Maybe (Maybe FilePath)) -> PrettyOpt a
mbPretty _ Nothing         = PrettyNone
mbPretty ppf (Just mbPath) = PrettyYes ppf mbPath

step :: PrettyOpt b -> (a -> Either Errs b) -> a -> IO b
step mbOpt f ast
  = case f ast of
      Left errs  -> errorExit errs
      Right ast' -> do case mbOpt of
                         PrettyNone           -> return ()
                         PrettyYes ppf mbPath -> dump ppf mbPath ast'
                       return ast'

errorExit :: Errs -> IO a
errorExit errs
  = do hPutStrLn stderr str
       exitWith (ExitFailure 1)
  where doc = ppErrs errs
        str = disp doc 9999 ""

dump :: (a -> PP_Doc) -> Maybe FilePath -> a -> IO ()
dump ppf mbPath x
  = case mbPath of
      Just f  -> writeFile f str
      Nothing -> putStrLn str
  where str = disp (ppf x) 9999 ""
