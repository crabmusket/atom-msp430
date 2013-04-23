module Language.Atom.MSP430 (
    module Language.Atom,
    module Language.Atom.MSP430.Watchdog,
    module Language.Atom.MSP430.Types,
    compileMSP430
 ) where

import Language.Atom
import Language.Atom.MSP430.Watchdog
import Language.Atom.MSP430.Types

compileMSP430 s l = do
    putStrLn "Compiling setup..."
    compile "setup" msp430defaults  s
    putStrLn "Compiling loop..."
    compile "loop"  msp430defaults l

msp430defaults = defaults { cRuleCoverage = False }
