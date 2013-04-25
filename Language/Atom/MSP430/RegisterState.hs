module Language.Atom.MSP430.RegisterState where

import Language.Atom
import Control.Monad.State
import Data.Bits

-- | Implements the State monad with a function to convert symbolic constants into
--   the type required by the state.
type RegisterState a b = State ((b, b), a -> b)

set, clear :: (Bits b) => a -> RegisterState a b ()
-- | Set bits of a RegisterState. In effect, s <- s || b.
set   b = state $ \((sets, clears), conv) -> ((), ((sets .|. conv b, clears), conv))
-- | Clear bits of a RegisterState. The effect is s <- s && ~b.
clear b = state $ \((sets, clears), conv) -> ((), ((sets, clears .|. conv b), conv))

-- | Clears all bits in a register.
reset :: (Bits b) => RegisterState a b ()
reset = state $ \((sets, clears), conv) -> ((), ((sets, clears .|. complement 0), conv))

-- | Run a register manipulation block on an empty register and return the resulting
--   register state.
execRegisterState :: (RegisterState a b ()) -> ((b, b), a -> b) -> (b, b)
execRegisterState fn init = fst $ execState fn init

-- | Update the state of a register given a block of commands. Any bits to 'clear' are cleared before
--   any bits to 'set' are set.
updateRegister :: (Assign b, IntegralE b, Bits b) => V b -> ((b, b), a -> b) -> RegisterState a b () -> Atom ()
updateRegister reg init s = let (sets, clears) = execRegisterState s init in
    case (sets, clears) of
        (0, 0) -> return ()
        (_, 0) -> reg <== BWOr (value reg) (Const sets)
        (0, _) -> reg <== BWAnd (value reg) (Const $ complement clears)
        otherwise -> reg <== BWOr (BWAnd (value reg) (Const $ complement clears)) (Const sets)

