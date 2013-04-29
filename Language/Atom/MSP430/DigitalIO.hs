module Language.Atom.MSP430.DigitalIO (
    IODir (..),
    IOValue (..),
    IORegister,
    IODirRegister,
    port1Out, port1Dir, port1In,
    port2Out, port2Dir, port2In
 ) where

import Language.Atom
import Language.Atom.MSP430.RegisterState
import Data.Bits
import Data.Word

-- | A type of RegisterState where you can set and clear IODirs.
type IODirRegister = RegisterState IODir Word8

-- | These options configure the state of an IO direction register. These registers
--   determine which pins act as inputs and which as outputs. Pins are input (0) by default;
--   to reset input after you've set output, simply clear (Out x).
data IODir
    = Out Int    -- ^ Puts a single pin in output mode.
    | Outs [Int] -- ^ Puts a list of pins in output mode.

-- | Convert an IODir symbol to an actual word value.
ioDirOptToWord8 :: IODir -> Word8
ioDirOptToWord8 o = case o of
    Out x   -> bit x
    Outs xs -> sum $ map bit xs

-- | A type of RegisterState where you can set and clear IOValues.
-- | Sets the direction of pins in an IO port.
portDir :: Int -> IODirRegister () -> Atom ()
portDir p s = updateRegister (portDir' p) ((0, 0), ioDirOptToWord8) s

-- | Gets a reference to a numbered output port's direction control register.
portDir' :: Int -> V Word8
portDir' p = word8' $ "P" ++ show p ++ "DIR"

type IORegister = RegisterState IOValue Word8

-- | Data constructor representing changes to the actual output state of a DIO pin. This
--   is separate from 'Out' to enforce meaningful syntax through the type system.
data IOValue
    = Pin Int    -- ^ Writes a single pin high.
    | Pins [Int] -- ^ Writes a list of pins high.

-- | Convert an IODir symbol to an actual word value.
ioValToWord8 :: IOValue -> Word8
ioValToWord8 o = case o of
    Pin x   -> bit x
    Pins xs -> sum $ map bit xs

-- | Sets the output values of the pins in an IO port.
portOut :: Int -> IORegister () -> Atom ()
portOut p s = updateRegister (portOut' p) ((0, 0), ioValToWord8) s

-- | Gets a reference to a numbered port's output register.
portOut' :: Int -> V Word8
portOut' p = word8' $ "P" ++ show p ++ "OUT"

-- | RegisterState for the pullup/down register enable register.
type IORENRegister = RegisterState IOResistor Word8

-- | Enable the pullup/down resistor of a pin.
data IOResistor
    = Resistor Int    -- ^ Enables a resistor on a single pin.
    | Resistors [Int] -- ^ Enables resistors for a list of pins.

-- | Enables the pullup/down resistors attached to an IO port.
portRen :: Int -> IORENRegister () -> Atom ()
portRen p = updateRegister (portRen' p) ((0, 0), ioResToWord8)

-- | Get a reference to a port's resistor enable register.
portRen' :: Int -> V Word8
portRen' p = word8' $ "P" ++ show p ++ "REN"

-- | Converts an IOResistor symbol to a word.
ioResToWord8 :: IOResistor -> Word8
ioResToWord8 r = case r of
    Resistor x   -> bit x
    Resistors xs -> sum $ map bit xs

type IOFunctionRegister = RegisterState IOFunction Word8

-- | Enumerates the functions an IO register can perform. The default is IOFunction.
data IOFunction
    = IOFunction
    | PrimaryPeripheral
    | Special
    | SecondaryPeripheral

ioFunToWord8 :: IOFunction -> Word8
ioFunToWord8 f = case f of
    IOFunction          -> 0x0000
    PrimaryPeripheral   -> 0x0001
    Special             -> 0x0002
    SecondaryPeripheral -> 0x0004

-- | Selects the function of an IO port.
portFun :: Int -> IOFunctionRegister () -> Atom ()
portFun p = updateRegister (portFun' p) ((0, 0), ioFunToWord8)

-- | Get a reference to the port's function select register.
portFun' :: Int -> V Word8
portFun' p = word8' $ "P" ++ show p ++ "SEL"

-- | Gets a reference to a numbered port's input register.
portIn' :: Int -> E Word8
portIn' p = value $ word8' $ "P" ++ show p ++ "IN"

port1In = portIn' 1
port1Out = portOut 1
port1Dir = portDir 1
port1Resistors = portRen 1
port1Function = portFun 1

port2In = portIn' 2
port2Out = portOut 2
port2Dir = portDir 2
port2Resistors = portRen 2
port2Function = portFun 2
