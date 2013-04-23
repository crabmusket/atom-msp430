module Language.Atom.MSP430.Types where

import Control.Monad.State
import Data.Bits

-- | Implements the State monad with a function to convert symbolic constants into
--   the type required by the state.
type RegisterState a b = State (b, a -> b)

set, clear :: (Bits b) => a -> RegisterState a b ()
-- | Set the bits of a RegisterState. In effect, s <- s || b.
set   b = state $ \(v, c) -> ((), (v .|. c b, c))
-- | Clear the bits of a RegisterState. The effect is s <- s && ~b.
clear b = state $ \(v, c) -> ((), (v .&. complement (c b), c))

