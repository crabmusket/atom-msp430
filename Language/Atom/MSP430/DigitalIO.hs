module Language.Atom.MSP430.DigitalIO where

import Language.Atom

port1In         = word8' "P1IN"  -- Read the value of PORT1 pins when used in input mode.
port1Out        = word8' "P1OUT" -- Set the values of PORT1 pins when used in output mode.
port1Dir        = word8' "P1DIR" -- Choose the input/output modes of PORT1 pins (1 = output).
port1Resistors  = word8' "P1REN" -- Enable pullup/pulldown resistors of PORT1 pins.
port1Function   = word8' "P1SEL" -- 

port1InterruptEnable = word8' "P1IE"      -- Enable interrupts for PORT1 pins in input mode.
port1InterruptFlags  = word8' "P1IFG"     -- Interrupt flags for PORT1 pins. A flag is set when an interrupt occurs; you should clear it manually.
port1InterruptEdgeSelect = word8' "P1IES" -- Interrupt edge select: if set to 1, the pin responds to a falling edge.

port2In         = word8' "P2IN"  -- Read the value of PORT2 pins when used in input mode.
port2Out        = word8' "P2OUT" -- Set the values of PORT2 pins when used in output mode.
port2Dir        = word8' "P2DIR" -- Choose the input/output modes of PORT2 pins (2 = output).
port2Resistors  = word8' "P2REN" -- Enable pullup/pulldown resistors of PORT2 pins.
port2Function   = word8' "P2SEL" -- 

port2InterruptEnable = word8' "P2IE"      -- Enable interrupts for PORT2 pins in input mode.
port2InterruptFlags  = word8' "P2IFG"     -- Interrupt flags for PORT2 pins. A flag is set when an interrupt occurs; you should clear it manually.
port2InterruptEdgeSelect = word8' "P2IES" -- Interrupt edge select: if set to 2, the pin responds to a falling edge.

