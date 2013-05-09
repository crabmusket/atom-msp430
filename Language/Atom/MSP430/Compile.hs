module Language.Atom.MSP430.Compile (
    MSP430Compilation (..),
    mspProgram, wiringProgram, simpleProgram, energiaProgram,
    mspCompile
 ) where

import Language.Atom
import Control.Monad
import System.IO

-- | Program information. Here we specify the functions that should be used in specific
--   roles in the compiled code.
data MSP430Compilation = MSP430Compilation {
    setupFn :: Maybe (Atom ()),
    setupFnName :: String,
    loopFn :: Maybe (Atom ()),
    loopFnName :: String,
    timerAInterrupt :: Maybe (Atom ()),
    timerAInterruptName :: String,
    watchdogInterrupt :: Maybe (Atom ()),
    watchdogInterruptName :: String,
    mainFile :: String,
    emitMainFn :: Bool,
    headers :: [String]
 }

-- | Default program to construct your own programs from. Contains Nothing and generates a
--   basic main.c.
mspProgram = MSP430Compilation {
    setupFn = Nothing,
    setupFnName = "setup",
    loopFn = Nothing,
    loopFnName = "loop",
    timerAInterrupt = Nothing,
    timerAInterruptName = "timerAISR",
    watchdogInterrupt = Nothing,
    watchdogInterruptName = "wdtISR",
    mainFile = "main.c",
    emitMainFn = True,
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

-- | Easy settings for a program with setup and loop, but no main function.
energiaProgram s l = mspProgram {
    setupFn = Just s,
    loopFn = Just l,
    emitMainFn = False
 }

-- | Compile a program given by the compilation specification. Compiles all functions into library files
--   and then generates a main file which calls these functions in the appropriate way.
mspCompile :: MSP430Compilation -> IO ()
mspCompile c = do
    let compile' = maybeCompile defaults {
        cRuleCoverage = False,
        cCode = \_ _ _ -> (unlines $ map (\h -> "#include \"" ++ h ++ ".h\"") (headers c), "")
     }
    compile' (setupFnName c) (setupFn c)
    compile' (loopFnName c) (loopFn c)
    compile' (timerAInterruptName c) (timerAInterrupt c)
    compile' (watchdogInterruptName c) (watchdogInterrupt c)
    putStrLn $ "Generating " ++ mainFile c ++ "..."
    hFlush stdout
    withFile (mainFile c) WriteMode $ \h -> do
        let put = hPutStrLn h
        let header' = maybeHeader h
        header' (setupFnName c) (setupFn c)
        header' (loopFnName c) (loopFn c)
        header' (timerAInterruptName c) (timerAInterrupt c)
        header' (watchdogInterruptName c) (watchdogInterrupt c)
        when (emitMainFn c) $ do
            put "\nint main(void) {"
            case setupFn c of
                Just f -> put $ "    " ++ setupFnName c ++ "();"
                Nothing -> return ()
            case loopFn c of
                Just f -> put $ "    while(1) " ++ loopFnName c ++ "();"
                Nothing -> return ()
            put "    return 0;"
            put "}\n"
        case timerAInterrupt c of
            Just fn -> do
                put   "#pragma vector=TIMERA0_VECTOR"
                put   "__interrupt void __timerA_isr(void) {"
                put $ "    " ++ timerAInterruptName c ++ "();"
                put   "}\n"
            Nothing -> return ()
        case watchdogInterrupt c of
            Just fn -> do
                put   "#pragma vector=WDT_VECTOR"
                put   "__interrupt void __wdt_isr(void) {"
                put $ "    " ++ watchdogInterruptName c ++ "();"
                put   "}\n"
            Nothing -> return ()
    return ()

maybeCompile s n f = case f of
    Nothing -> return ()
    Just fn -> do
        putStrLn $ "Compiling " ++ n ++ "..."
        hFlush stdout
        compile n s fn
        return ()

maybeHeader h header f = case f of
    Just _ -> hPutStrLn h $ "#include \"" ++ header ++ ".h\""
    Nothing -> return ()

