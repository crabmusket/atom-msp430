module Language.Atom.MSP430.DigitalIO (
    IOOption (..),
    IORegister,
    portDir
 ) where

import Language.Atom
import Language.Atom.MSP430.RegisterState
import Data.Bits
import Data.Word

-- | A type of RegisterState where you can set and clear IOOptions.
type IORegister = RegisterState IOOption Word8

-- | These options configure the state of an IO direction register. These registers
--   determine which pins act as inputs and which as outputs. Pins are input (0) by default;
--   to reset input after you've set output, simply clear (Out x).
data IOOption
    = Out Int    -- ^ Puts a single pin in output mode.
    | Outs [Int] -- ^ Puts a list of pins in output mode.

-- | Convert an IOOption symbol to an actual word value.
ioOptToWord8 :: IOOption -> Word8
ioOptToWord8 o = case o of
    Out x   -> shift 0x01 x
    Outs xs -> sum $ map (shift 0x01) xs

portDir :: Int -> IORegister () -> Atom ()
portDir p s = port <== (Const $ execRegisterState s (0, ioOptToWord8))
    where port = word8' $ "P" ++ show p ++ "DIR"

