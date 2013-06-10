WDT Interrupt
-------------

_This is a [literate Haskell][LHS] file, which means GHC can read it as valid Haskell code.
Try `runhaskell interrupt.lhs` and see!_

> module Main where
> import Language.Atom.MSP430
> 
> setup = do
>     atom "settings" $ do
>         watchdog <== Const (wdtPassword .|. wdtIntervalMode)
>         port1Dir <== Const (complement 0)
>         port1Out <== Const 0x0001
>         interruptEnable <== Const wdtInterruptEnable
>         call "__enable_interrupt"
>     atom "poweroff" $ action (\_ -> "_BIS_SR(LPM0_bits|GIE)") []
> 
> isr = do
>     incr port1Out
> 
> main = mspCompile "g2231" $ mspProgram {
>     setupFn = Just setup,
>     watchdogISR = Just isr
>  }

 [LHS]: http://www.haskell.org/haskellwiki/Literate_programming
