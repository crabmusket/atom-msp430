module Language.Atom.MSP430.Compile (
    MSP430Compilation (..),
    program,
    mspCompile
 ) where

import Language.Atom
import Control.Monad
import System.IO

nullAtom :: Atom ()
nullAtom = return ()

-- | Program information. It specifies the functions that should be used in specific
--   roles in the compiled code, as well as other configuration information.
data MSP430Compilation = MSP430Compilation {
    setup :: Atom (),       -- ^ Function called once when the MCU starts up.
    loop :: Atom (),        -- ^ Function called in a busy loop after setup.
    timerAISR :: Atom (),   -- ^ Function to run when a TimerA CCR interrupt happens.
    watchdogISR :: Atom (), -- ^ Function to call when the WDT interrupts.
    port1ISR :: Atom (),    -- ^ Function to call when there is a PORT1 interrupt.
    port2ISR :: Atom ()    -- ^ Function to call when there is a PORT2 interrupt.
 }

-- | Default program to construct your own programs from. Contains all null atoms.
program :: MSP430Compilation
program = MSP430Compilation {
    setup = nullAtom,
    loop = nullAtom,
    timerAISR = nullAtom,
    watchdogISR = nullAtom,
    port1ISR = nullAtom,
    port2ISR = nullAtom
 }

-- | Compile a program given by the compilation specification. Compiles all functions into library files
--   and then generates a main file which calls these functions in the appropriate way.
mspCompile :: String -> MSP430Compilation -> IO ()
mspCompile h c = do
    let headers = unlines $ map (\h -> "#include \"msp430" ++ h ++ ".h\"") [h]
    let compile' = maybeCompile defaults {
        cRuleCoverage = False,
        cAssert = False,
        cCode = \_ _ _ -> (headers, "")
     }
    compile' "setup" (setup c)
    compile' "loop" (loop c)
    compile' "timerAISR" (timerAISR c)
    compile' "watchdogISR" (watchdogISR c)
    compile' "port1ISR" (port1ISR c)
    compile' "port2ISR" (port2ISR c)
    putStrLn $ "Generating main.c ..."
    hFlush stdout
    withFile ("main.c") WriteMode $ \h -> do
        let put = hPutStrLn h
        let header' = maybeHeader h
        let interrupt' = maybeInterrupt h
        header' "setup" (setup c)
        header' "loop" (loop c)
        header' "timerAISR" (timerAISR c)
        header' "watchdogISR" (watchdogISR c)
        header' "port1ISR" (port1ISR c)
        header' "port2ISR" (port2ISR c)
        put headers
        put "\nint main(void) {"
        case setup c of
            nullAtom -> return ()
            otherwise -> put $ "    setup();"
        case loop c of
            nullAtom -> return ()
            otherwise -> put $ "    while(1) loop();"
        put "    return 0;"
        put "}\n"
        interrupt' "timerAISR"   (timerAISR c)   "TIMERA0_VECTOR"
        interrupt' "watchdogISR" (watchdogISR c) "WDT_VECTOR"
        interrupt' "port1ISR"    (port1ISR c)    "PORT1_VECTOR"
        interrupt' "port2ISR"    (port2ISR c)    "PORT2_VECTOR"
    return ()

maybeCompile s n f = case f of
    nullAtom -> return ()
    otherwise -> do
        putStrLn $ "Compiling " ++ n ++ "..."
        hFlush stdout
        compile n s f
        return ()

maybeHeader h header f = case f of
    nullAtom -> return ()
    otherwise -> hPutStrLn h $ "#include \"" ++ header ++ ".h\""

maybeInterrupt h n f v = case f of
    nullAtom -> return ()
    otherwise -> do
        let put = hPutStrLn h
        put $ "#pragma vector=" ++ v
        put $ "__interrupt void __" ++ n ++ "(void) {"
        put $ "    " ++ n ++ "();"
        put   "}\n"

