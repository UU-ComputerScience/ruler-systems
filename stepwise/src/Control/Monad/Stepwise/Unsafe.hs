{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Control.Monad.Stepwise.Unsafe
  ( inlinePerformIO
  , unsafeCoerce
  , ghc7compat) where

import GHC.Base
import GHC.IO
import Unsafe.Coerce


-- | Runs the I/O computation when the value is needed.
--   The effects may be duplicated when the value itself is duplicated
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m)
  = case m realWorld# of
      (# _, r #) -> r

{-# INLINE inlinePerformIO #-}


ghc7compat :: a -> b
ghc7compat = unsafeCoerce

{-# INLINE ghc7compat #-}