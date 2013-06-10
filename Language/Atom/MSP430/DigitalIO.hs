module Language.Atom.MSP430.DigitalIO where

import Language.Atom

port1In         = word8' "P1IN"
port1Out        = word8' "P1OUT"
port1Dir        = word8' "P1DIR"
port1Resistors  = word8' "P1REN"
port1Function   = word8' "P1SEL"

port1InterruptEnable = word8' "P1IE"
port1InterruptFlags  = word8' "P1IFG"
port1InterruptEdgeSelect = word8' "P1IES"

port2In         = word8' "P2IN"
port2Out        = word8' "P2OUT"
port2Dir        = word8' "P2DIR"
port2Resistors  = word8' "P2REN"
port2Function   = word8' "P2SEL"

port2InterruptEnable = word8' "P2IE"
port2InterruptFlags  = word8' "P2IFG"
port2InterruptEdgeSelect = word8' "P2IES"

