module Util.Options (Opts(..), getOptions, options) where

import System.Console.GetOpt


options :: [OptDescr (Opts -> Opts)]
options
  = [ Option ['v']    ["verbose"]    (NoArg verboseOpt)    "verbose error message format"
    ]


data Opts = Opts
  { hasVerboseFlag :: Bool
  }


defaults :: Opts
defaults = Opts
  { hasVerboseFlag = False
  }


verboseOpt opts = opts { hasVerboseFlag = True }


getOptions :: [String] -> (Opts, [String], [String])
getOptions args
  = let (flags, sources, errors) = getOpt Permute options args
    in (foldl (flip ($)) defaults flags, sources, errors)

