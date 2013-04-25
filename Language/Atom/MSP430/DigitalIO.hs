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
    = Out Int     -- ^ Puts a single pin in output mode.
    | Bit Int     -- ^ Identical to Out.
    | High Int    -- ^ Identical to Out.
    | Outs [Int]  -- ^ Puts a list of pins in output mode.
    | Bits [Int]  -- ^ Identical to Outs.
    | Highs [Int] -- ^ Identical to Outs.

-- | Convert an IOOption symbol to an actual word value.
ioOptToWord8 :: IOOption -> Word8
ioOptToWord8 o = case o of
    Out x    -> bit x
    Bit x    -> bit x
    High x   -> bit x
    Outs xs  -> sum $ map (bit) xs
    Bits xs  -> sum $ map (bit) xs
    Highs xs -> sum $ map (bit) xs

portDir :: Int -> IORegister () -> Atom ()
portDir p s = updateRegister (portDir' p) ((0, 0), ioOptToWord8) s

-- | Gets a reference to a numbered output port's direction control register.
portDir' :: Int -> V Word8
portDir' p = word8' $ "P" ++ show p ++ "DIR"

portOut :: Int -> IORegister () -> Atom ()
portOut p s = updateRegister (portOut' p) ((0, 0), ioOptToWord8) s

-- | Gets a reference to a numbered port's output register.
portOut' :: Int -> V Word8
portOut' p = word8' $ "P" ++ show p ++ "OUT"

-- | Gets a reference to a numbered port's input register.
portIn' :: Int -> E Word8
portIn' p = value $ word8' $ "P" ++ show p ++ "IN"
