{-# LANGUAGE BangPatterns #-}

-- | Some benchmarks of the Stepwise monad.
module Main(main) where

import Control.Monad.Stepwise.Core
import Control.Monad.Identity

import Criterion.Types
import Progression.Main


data I t = I

bindbench :: Monad m => Int -> Int -> m ()
bindbench length depth
  = tree depth
  where
    runner !0 !n = return ()
    runner m n = tree n >>= \() -> runner (m-1) n
    
    tree !0 = return ()
    tree n = runner length (n-1)

bindbench' :: Int -> Int -> Stepwise AnyFailure I any AnyWatcher ()
bindbench' = bindbench

{-# INLINE bindbench #-}
{-# INLINE bindbench' #-}


bench1 :: Int -> Int -> IO ()
bench1 length size = seq (runIdentity (bindbench length size)) (return ())

bench2 :: Int -> Int -> IO ()
bench2 length size = seq (lazyEval (bindbench' length size)) (return ())

bench3 :: Int -> Int -> IO ()
bench3 length size = seq (seqEval (bindbench' length size)) (return ())

bench4 :: Int -> Int -> IO ()
bench4 length size = seq (stepwiseEval (bindbench' length size)) (return ())


listbenchmarks :: [(String, IO ())]
listbenchmarks = [ ("list-" ++ n ++ "-" ++ show p ++ "-" ++ show q, f p q) | (p,q) <- params, (n,f) <- funs ]
  where
    params = [ (10000,1)
             , (100000,1)
             , (1000000,1)
             , (2, 5)
             , (2, 7)
             , (2, 9)
             , (3, 5)
             , (50, 3)
             , (10, 4)
             ]
    funs = [ ("id",   bench1)
           , ("lazy", bench2)
--           , ("seq",  bench3)
           , ("step", bench4) ]


main :: IO ()
main = defaultMain $ bgroup "stepwise" $ map (uncurry bench) listbenchmarks
