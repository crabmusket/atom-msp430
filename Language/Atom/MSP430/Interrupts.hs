module Language.Atom.MSP430.Interrupts where

import Language.Atom

interruptEnable = word8' "IE1"  -- Global interrupt vector.
interruptFlags  = word8' "IFG1" -- Global interrupt flags.
