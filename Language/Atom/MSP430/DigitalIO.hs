module Language.Atom.MSP430.DigitalIO (
    IOOption (..),
    IORegister,
    portDir, portDir',
    portOut, portOut',
    portIn'
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
    | Bit Int    -- ^ Identical to Out.
    | Outs [Int] -- ^ Puts a list of pins in output mode.
    | Bits [Int] -- ^ Identical to Outs.

-- | Convert an IOOption symbol to an actual word value.
ioOptToWord8 :: IOOption -> Word8
ioOptToWord8 o = case o of
    Out x   -> shift 0x01 x
    Bit x   -> shift 0x01 x
    Outs xs -> sum $ map (shift 0x01) xs
    Bits xs -> sum $ map (shift 0x01) xs

portDir :: Int -> IORegister () -> Atom ()
portDir p s = portDir' p <== (Const $ execRegisterState s (0, ioOptToWord8))

-- | Gets a reference to a numbered output port's direction control register.
portDir' :: Int -> V Word8
portDir' p = word8' $ "P" ++ show p ++ "DIR"

portOut :: Int -> IORegister () -> Atom ()
portOut p s = portOut' p <== (Const $ execRegisterState s (0, ioOptToWord8))

-- | Gets a reference to a numbered port's output register.
portOut' :: Int -> V Word8
portOut' p = word8' $ "P" ++ show p ++ "OUT"

-- | Gets a reference to a numbered port's input register.
portIn' :: Int -> E Word8
portIn' p = value $ word8' $ "P" ++ show p ++ "IN"
