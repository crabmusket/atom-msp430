module Language.Atom.MSP430.Types where

import Control.Monad.State
import Data.Bits

type RegisterState a b = State (b, a -> b)

set, clear :: (Bits b) => a -> RegisterState a b ()
set   b = state $ \(v, c) -> ((), (v .|. c b, c))
clear b = state $ \(v, c) -> ((), (v .&. complement (c b), c))

