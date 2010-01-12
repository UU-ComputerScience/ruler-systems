module Opts where

import System.Console.GetOpt
import qualified Control.Monad.Error
import Error
import qualified Data.Sequence as Seq


data Opts
  = Opts { optDumpTks   :: Maybe (Maybe FilePath)
         , optDumpFront :: Maybe (Maybe FilePath)
         , optOutput    :: Maybe FilePath
         , optSource    :: FilePath
         }

defaultOpts
  = Opts { optDumpTks   = Nothing
         , optDumpFront = Nothing
         , optOutput    = Nothing
         , optSource    = "undefined file"
         }

options :: [OptDescr (Opts -> Either Error Opts)]
options
  = [ Option [] ["dump-tks"]
        (OptArg (\mbPath opts -> return $ opts { optDumpTks = Just mbPath } ) "FILE")
        "Dump tokens of source to stdout or FILE"
    , Option [] ["dump-front"]
        (OptArg (\mbPath opts -> return $ opts { optDumpFront = Just mbPath } ) "FILE")
        "Dump tokens of the Front AST to stdout or FILE"
    , Option ['o'] ["output"]
        (ReqArg (\f opts -> return $ opts { optOutput = Just f })
                "FILE"
        )
        "Output FILE"
    ]

parseOpts :: [String] -> Either Errs Opts
parseOpts args
  = case getOpt Permute options args of
      (o,[n],[]) -> case foldl (>>=) (return (defaultOpts { optSource = n })) o of
                      Left e  -> Left (Seq.singleton e)
                      Right r -> Right r
      (_,_,[])   -> Left (Seq.singleton $ Error_Cmdline ["no source file specified"] info)
      (_,_,errs) -> Left (Seq.singleton $ Error_Cmdline errs info)
  where
    header = "Usage: vag [OPTION...] FILE"
    info   = usageInfo header options
