module Language.Atom.MSP430.Interrupts where

import Language.Atom

interruptEnable = word16' "IE1"
interruptFlags  = word16' "IFG1"
