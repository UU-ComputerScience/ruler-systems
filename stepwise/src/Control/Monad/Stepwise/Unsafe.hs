{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Control.Monad.Stepwise.Unsafe
  ( inlinePerformIO
  , unsafeCoerce) where

import GHC.Base
import GHC.IO
import Unsafe.Coerce


{-# INLINE inlinePerformIO #-} 
inlinePerformIO :: IO a -> a 
inlinePerformIO (IO m)
  = case m realWorld# of
      (# _, r #) -> r
