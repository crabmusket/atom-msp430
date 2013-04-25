module Language.Atom.MSP430.Watchdog (
    WatchdogOption (..),
    WatchdogRegister,
    watchdog, watchdog'
 ) where

import Language.Atom
import Language.Atom.MSP430.RegisterState
import Data.Word

-- | A type of RegisterState where you can set and clear WatchdogOptions.
type WatchdogRegister = RegisterState WatchdogOption Word16

-- | These options control the operation of the watchdog timer. Any of these options may be set or cleared, but
--   remember to always set the Password when issuing instructions to the watchdog timer.
data WatchdogOption
    = Password      -- ^ Password to access the options register. Set this every time you touch the watchdog timer!
    | Hold          -- ^ If set, the WDT is suspended and does not perform its normal role of resetting the uC.
    | NMIFalling    -- ^ When set, the uC resets on a falling edge from the RST/NMI pin, not a rising edge.
    | NMI           -- ^ Enable NMI interrupt according to NMIFalling.
    | IntervalMode  -- ^ Use the WDT like a regular timer with an interrupt instead of letting it reset the uC.
    | ClearCounter  -- ^ Setting this bit resets the WDT count to 0.
    | UseAuxClock   -- ^ Set this to use the auxiliary/externall ACLK instead of the SMCLK as the watchdog timer source.
    | SourceDiv15   -- ^ Divide clock input rate by 2^15 (32768).
    | SourceDiv13   -- ^ Divide clock input rate by 2^13 (8192).
    | SourceDiv9    -- ^ Divide clock input rate by 2^9 (512).
    | SourceDiv6    -- ^ Divide clock input rate by 2^6 (64).

-- | Convert a WatchdogOption symbol to an actual word value.
wdtOptToWord16 :: WatchdogOption -> Word16
wdtOptToWord16 o = case o of
    Password      -> 0x5A00
    Hold          -> 0x0080
    NMIFalling    -> 0x0040
    NMI           -> 0x0020
    IntervalMode  -> 0x0010
    ClearCounter  -> 0x0008
    UseAuxClock   -> 0x0004
    SourceDiv15   -> 0x0000
    SourceDiv13   -> 0x0001
    SourceDiv9    -> 0x0002
    SourceDiv6    -> 0x0003

-- | Reference to the WDTCTL register that controls the behavior of the watchdog timer.
wdtctl = word16' "WDTCTL"

watchdog, watchdog' :: WatchdogRegister () -> Atom ()
-- | Set options in the watchdog timer. Give it a series of 'set' and 'clear' instructions using values of the
--   'WatchdogOption' type. Internally, it operates on the bits of the WDTCTL register.
watchdog s = updateRegister wdtctl ((0, 0), wdtOptToWord16) s
-- | Functions identically to 'watchdog', but you don't have to manually set 'Password'.
watchdog' s = updateRegister wdtctl ((0x5A00, 0), wdtOptToWord16) s
