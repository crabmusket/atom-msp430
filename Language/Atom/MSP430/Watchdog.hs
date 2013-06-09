module Language.Atom.MSP430.Watchdog where

import Language.Atom
import Data.Word

wdtPassword      = 0x5A00 :: Word16
wdtHold          = 0x0080 :: Word16
wdtNMIFalling    = 0x0040 :: Word16
wdtNMI           = 0x0020 :: Word16
wdtIntervalMode  = 0x0010 :: Word16
wdtClearCounter  = 0x0008 :: Word16
wdtUseAuxClock   = 0x0004 :: Word16
wdtSourceDiv15   = 0x0000 :: Word16
wdtSourceDiv13   = 0x0001 :: Word16
wdtSourceDiv9    = 0x0002 :: Word16
wdtSourceDiv6    = 0x0003 :: Word16

watchdog = word16' "WDTCTL"
