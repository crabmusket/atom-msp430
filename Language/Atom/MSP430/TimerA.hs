module Language.Atom.MSP430.TimerA where

import Language.Atom
import Data.Word

taSource           = 0x0000 :: Word16
taSourceACLK       = 0x0100 :: Word16
taSourceSMCLK      = 0x0200 :: Word16
taSourceINCLK      = 0x0300 :: Word16
taClear            = 0x0004 :: Word16
taInterruptEnabled = 0x0002 :: Word16
taStopMode         = 0x0000 :: Word16
taUpMode           = 0x0010 :: Word16
taContinuousMode   = 0x0020 :: Word16
taUpDownMode       = 0x0030 :: Word16
taSourceDiv8       = 0x00C0 :: Word16
taSourceDiv4       = 0x0080 :: Word16
taSourceDiv2       = 0x0040 :: Word16
taSourceDiv1       = 0x0000 :: Word16

taCCRInterrupt = 0x0010 :: Word16

timerAControl = word16' "TACTL"
timerACCR0 = word16' "TACCR0"
timerACCC0 = word16' "TACCTL0"
