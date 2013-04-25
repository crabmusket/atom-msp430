module Language.Atom.MSP430.Compile where

import Language.Atom
import Control.Monad
import System.IO

-- | Program information. Here we specify the functions that should be used in specific
--   roles in the compiled code.
data MSP430Compilation = MSP430Compilation {
    setupFn :: Maybe (Atom ()),
    loopFn :: Maybe (Atom ()),
    mainFile :: String,
    headers :: [String]
 }

-- | Default program to construct your own programs from. Contains Nothing and generates a
--   basic main.c.
mspProgram = MSP430Compilation {
    setupFn = Nothing,
    loopFn = Nothing,
    mainFile = "main.c",
    headers = []
 }

-- | Easy settings for a Wiring-style program with setup and loop functions. Expects a device extension
--   for header files - i.e. running with "g2231" wihh generate files that #include "msp430g2231.h"
wiringProgram h s l = mspProgram {
    setupFn = Just s,
    loopFn = Just l,
    headers = ["msp430" ++ h]
 }

-- | Easy settings for a program with just a setup function.
simpleProgram h s = mspProgram {
    setupFn = Just s,
    headers = ["msp430" ++ h]
 }

-- | Compile a program given by the compilation specification. Compiles all functions into library files
--   and then generates a main file which calls these functions in the appropriate way.
mspCompile :: MSP430Compilation -> IO ()
mspCompile c = do
    let msp430defaults = defaults {
        cRuleCoverage = False,
        cCode = \_ _ _ -> (unlines $ map (\h -> "#include \"" ++ h ++ ".h\"") (headers c), "")
     }
    case (setupFn c) of
        Nothing -> return ()
        Just f -> do
            putStrLn "Compiling setup function..."
            hFlush stdout
            compile "setup" msp430defaults f
            return ()
    case (loopFn c) of
        Nothing -> return ()
        Just f -> do
            putStrLn "Compiling loop function..."
            hFlush stdout
            compile "loop"  msp430defaults f
            return ()
    putStrLn "Generating main file..."
    hFlush stdout
    return ()

