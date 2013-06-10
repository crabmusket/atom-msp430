module Language.Atom.MSP430.Interrupts where

import Language.Atom

interruptEnable = word8' "IE1"
interruptFlags  = word8' "IFG1"
