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

-- | 
mspProgram = MSP430Compilation {
    setupFn = Nothing,
    loopFn = Nothing,
    mainFile = "main.c",
    headers = []
 }

-- | Compile a Wiring-like program for the MSP430 with Atom functions for setup and loop.
mspCompile :: MSP430Compilation -> IO ()
mspCompile c = do
    let msp430defaults = defaults {
        cRuleCoverage = False,
        cCode = \_ _ _ -> (unlines $ map (\h -> "#include \"" ++ h ++ "\"") (headers c), "")
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
    return ()

mspWiring h s l = mspProgram {
    setupFn = Just s,
    loopFn = Just l,
    headers = ["msp430" ++ h]
 }

mspSimple h s = mspProgram {
    setupFn = Just s,
    headers = ["msp430" ++ h]
 }

