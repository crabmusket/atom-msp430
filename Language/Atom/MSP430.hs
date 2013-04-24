module Language.Atom.MSP430 (
    module Language.Atom,
    module Language.Atom.MSP430.RegisterState,
    module Language.Atom.MSP430.Watchdog,
    module Language.Atom.MSP430.DigitalIO,
    compileMSP430
 ) where

import Language.Atom
import Language.Atom.MSP430.RegisterState
import Language.Atom.MSP430.Watchdog
import Language.Atom.MSP430.DigitalIO

-- | Compile a Wiring-like program for the MSP430 with Atom functions for setup and loop.
compileMSP430 :: String -> Atom () -> Atom () -> IO ()
compileMSP430 model setup loop = do
    let msp430defaults = defaults {
        cRuleCoverage = False,
        cCode = \_ _ _ -> ("#include <msp430" ++ model ++ ".h>", "")
     }
    putStrLn "Compiling setup..."
    compile "setup" msp430defaults setup
    putStrLn "Compiling loop..."
    compile "loop"  msp430defaults loop
    return ()

