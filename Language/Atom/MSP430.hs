module Language.Atom.MSP430 (
    module Language.Atom,
    module Language.Atom.MSP430.RegisterState,
    module Language.Atom.MSP430.Watchdog,
    compileMSP430
 ) where

import Language.Atom
import Language.Atom.MSP430.RegisterState
import Language.Atom.MSP430.Watchdog

-- | Compile a Wiring-like program for the MSP430 with Atom functions for setup and loop.
compileMSP430 s l = do
    putStrLn "Compiling setup..."
    compile "setup" msp430defaults  s
    putStrLn "Compiling loop..."
    compile "loop"  msp430defaults l

-- | Default options for MSP430 C code generation. In this case, just disable coverage checking.
msp430defaults = defaults { cRuleCoverage = False }
