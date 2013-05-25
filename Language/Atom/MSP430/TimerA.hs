module Language.Atom.MSP430.TimerA (
 ) where

import Language.Atom
import Language.Atom.MSP430.RegisterState
import Data.Word

-- | A type of RegisterState where you can set and clear WatchdogOptions.
type TimerAControlRegister = RegisterState TimerAOption Word16

-- | These options control the operation of the watchdog timer. Any of these options may be set or cleared, but
--   remember to always set the Password when issuing instructions to the watchdog timer.
data TimerAOption
    = Clear         -- ^ Resets the timer count and clock divider.
    | InterruptEnabled -- ^ 
    | StopMode      -- ^ 
    | UpMode        -- ^ 
    | ContinuousMode -- ^ 
    | UpDownMode    -- ^ 
    | SourceDiv8    -- ^ Divide clock input rate by 8.
    | SourceDiv4    -- ^ Divide clock input rate by 4.
    | SourceDiv2    -- ^ Divide clock input rate by 2.
    | SourceDiv1    -- ^ Divide clock input rate by 1.

-- | Convert a TimerAOption symbol to an actual word value.
taOptToWord16 :: TimerAOption -> Word16
taOptToWord16 o = case o of
    Clear         -> 0x0004
    InterruptEnabled -> 0x0002
    StopMode      -> 0x0
    UpMode        -> 0x0010
    ContinuousMode -> 0x0020
    UpDownMode    -> 0x0030
    SourceDiv8    -> 0x00C0
    SourceDiv4    -> 0x0080
    SourceDiv2    -> 0x0040
    SourceDiv1    -> 0x0000

-- | Reference to the TACTL register that controls the behavior of Timer A.
timerAControl = word16' "TACTL"

timerA :: TimerAControlRegister () -> Atom ()
timerA s = updateRegister timerAControl ((0, 0), taOptToWord16) s
