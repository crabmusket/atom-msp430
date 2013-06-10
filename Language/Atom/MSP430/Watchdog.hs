module Language.Atom.MSP430.Watchdog where

import Language.Atom
import Data.Word

wdtPassword      = 0x5A00 :: Word16 -- ^ Password bits to change the waychdog settings. Include this in every write to 'watchdog'.
wdtHold          = 0x0080 :: Word16 -- ^ Stop the watchdog timer.
wdtNMIFalling    = 0x0040 :: Word16 -- ^ NMI interrupt on falling edge.
wdtNMI           = 0x0020 :: Word16 -- ^ NMI interrupt.
wdtIntervalMode  = 0x0010 :: Word16 -- ^ Use the timer as an interval counter with interrupt.
wdtClearCounter  = 0x0008 :: Word16 -- ^ Clear the WDT counter when in interval mode.
wdtUseAuxClock   = 0x0004 :: Word16 -- ^ Source WDT from ACLK.
wdtSourceDiv15   = 0x0000 :: Word16 -- ^ Divide the timer input by 2^15.
wdtSourceDiv13   = 0x0001 :: Word16 -- ^ Divide the timer input by 2^13.
wdtSourceDiv9    = 0x0002 :: Word16 -- ^ Divide the timer input by 2^9.
wdtSourceDiv6    = 0x0003 :: Word16 -- ^ Divide the timer input by 2^6.

wdtInterruptEnable    = 0x01 :: Word8 -- ^ Enable the WDT interrupt in 'interruptEnable'.
wdtNMIInterruptEnable = 0x10 :: Word8 -- ^ Enable the WDT's NMI interrupt in 'interruptEnable'.

watchdog = word16' "WDTCTL" -- ^ Watchdog control register.
