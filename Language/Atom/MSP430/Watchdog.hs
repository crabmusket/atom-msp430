module Language.Atom.MSP430.Watchdog (
    WatchdogOption (..),
    watchdog
 ) where

import Language.Atom
import Language.Atom.MSP430.Types
import Data.Word
import Control.Monad.State

data WatchdogOption
    = Password      -- 
    | Hold          --
    | NMIFalling    --
    | NMI           --
    | IntervalMode  --
    | ClearCounter  --
    | UseACLK       --
    | InterruptMode --

wdtOptToWord16 :: WatchdogOption -> Word16
wdtOptToWord16 o = case o of
    Password      -> 0x5A00
    Hold          -> 0x0080
    NMIFalling    -> 0x0040
    NMI           -> 0x0020
    IntervalMode  -> 0x0010
    ClearCounter  -> 0x0008
    UseACLK       -> 0x0004
    InterruptMode -> 0x04

wdtctl = word16' "WDTCTL"

watchdog :: (RegisterState WatchdogOption Word16 ()) -> Atom ()
watchdog s = wdtctl <== (Const $ fst $ execState s (0, wdtOptToWord16))

