-- | Module for use in combination
--   with UUAG's "--breadtfirst" option.

{-# LANGUAGE TypeFamilies, BangPatterns #-}
module Control.Monad.Stepwise.AG
 ( module Control.Monad.Stepwise
 , Child(Child)
 , Inh, Syn, Comp
 , invoke
 ) where

import Control.Monad.Stepwise  -- get all stepwise functionality

-- | Semantics of a child of type @n@ as a function from inherited
-- attributes (@Inh n@) to a computation @Comp i n@ of synthesized attributes (@Syn n@).
newtype Child i n = Child (Inh n -> Comp i n)
data family Inh n :: *         -- index @n@ uniquely determines the type of the inherited and
data family Syn n :: *         -- synthesized attributes.

-- | We use slightly simpler stepwise computations for AGs.
type Comp i n = Stepwise AnyFailure i Lazy AnyWatcher (Syn n)

-- | Unwraps a @Closure@
invoke :: Child i n -> Inh n -> Comp i n
invoke (Child f) inh = f inh
