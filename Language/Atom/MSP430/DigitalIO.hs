module Language.Atom.MSP430.DigitalIO (
    port1In, port1Out, port1Dir, port1Resistors, port1Function,
    port2In, port2Out, port2Dir, port2Resistors, port2Function
 ) where

import Language.Atom

port1In         = word8' "P1IN"
port1Out        = word8' "P1OUT"
port1Dir        = word8' "P1DIR"
port1Resistors  = word8' "P1REN"
port1Function   = word8' "P1SEL"

port2In         = word8' "P2IN"
port2Out        = word8' "P2OUT"
port2Dir        = word8' "P2DIR"
port2Resistors  = word8' "P2REN"
port2Function   = word8' "P2SEL"
