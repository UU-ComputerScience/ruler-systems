-- | Some benchmarks of the Stepwise monad.
module Main(main) where

import Control.Monad.Stepwise.Core
import Control.Monad.Identity

import Criterion.Types
import Progression.Main


data I t = I

listbench :: Monad m => Int -> m ()
listbench size
  = do x <- runner 0 list
       seq x (return ())
  where
    list = [1..size]
    
    runner acc [] = return acc
    runner acc (x:xs) = (return acc) >>= \acc' -> seq acc' $ runner (max x acc') xs

listbench' :: Int -> Stepwise AnyFailure I any AnyWatcher ()
listbench' size = listbench size

{-# INLINE listbench #-}
{-# INLINE listbench' #-}


listbench1 :: Int -> IO ()
listbench1 size = seq (runIdentity (listbench size)) (return ())

listbench2 :: Int -> IO ()
listbench2 size = seq (lazyEval (listbench' size)) (return ())

listbench3 :: Int -> IO ()
listbench3 size = seq (seqEval (listbench' size)) (return ())

listbench4 :: Int -> IO ()
listbench4 size = seq (stepwiseEval (listbench' size)) (return ())


listbenchmarks :: [(String, IO ())]
listbenchmarks = [ ("list-" ++ n ++ "-" ++ show p, f p) | p <- params, (n,f) <- funs ]
  where
    params = [10000,100000,1000000]
    funs = [ ("id",   listbench1)
           , ("lazy", listbench2)
           , ("seq",  listbench3)
           , ("step", listbench4) ]


main :: IO ()
main = defaultMain $ bgroup "stepwise" $ map (uncurry bench) listbenchmarks
