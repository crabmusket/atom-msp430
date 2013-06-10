module Language.Atom.MSP430.TimerA where

import Language.Atom
import Data.Word

taSource           = 0x0000 :: Word16 -- ^ Source Timer A from TACLK.
taSourceACLK       = 0x0100 :: Word16 -- ^ Source Timer A from ACLK.
taSourceSMCLK      = 0x0200 :: Word16 -- ^ Source Timer A from SMCLK.
taSourceINCLK      = 0x0300 :: Word16 -- ^ Source Timer A from INCLK.
taClear            = 0x0004 :: Word16 -- ^ Clear Timer A (reset it to 0).
taInterruptEnabled = 0x0002 :: Word16 -- ^ Enable the Timer A interrupt.
taStopMode         = 0x0000 :: Word16 -- ^ Stop the timer from counting.
taUpMode           = 0x0010 :: Word16 -- ^ Timer counts from 0 to TACCR0.
taContinuousMode   = 0x0020 :: Word16 -- ^ Timer counts from 0 to 0xFFFF.
taUpDownMode       = 0x0030 :: Word16 -- ^ Timer counts from 0 to TACCR0 to 0.
taSourceDiv1       = 0x0000 :: Word16 -- ^ Divide timer input source by 1.
taSourceDiv2       = 0x0040 :: Word16 -- ^ Divide timer input source by 2.
taSourceDiv4       = 0x0080 :: Word16 -- ^ Divide timer input source by 4.
taSourceDiv8       = 0x00C0 :: Word16 -- ^ Divide timer input source by 8.

taCCRInterrupt = 0x0010 :: Word16 -- ^ Interrupt on a Timer A CCR.

timerAControl = word16' "TACTL" -- ^ Timer A control register.
timerACCR0 = word16' "TACCR0"   -- ^ Timer A capture/compare register 0.
timerACCC0 = word16' "TACCTL0"  -- ^ Timer A capture/compare control register 0.
