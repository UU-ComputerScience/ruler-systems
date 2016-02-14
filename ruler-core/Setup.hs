module Main where

import Distribution.Simple (defaultMainWithHooks)
import Distribution.Simple.UUAGC (uuagcUserHook')
import UU.UUAGC (uuagc)

main :: IO ()
main = defaultMainWithHooks $
         uuagcUserHook' "C:\\projects\\uuagc\\dist\\build\\uuagc\\uuagc.exe"
