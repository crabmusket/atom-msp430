Button interrupt
----------------

_This is a [literate Haskell][LHS] file, which means GHC can read it as valid Haskell code.
Try `runhaskell buttonInterrupt.lhs` and see!_

> module Main where
> import Language.Atom.MSP430
> 
> button = 0x08
> 
> setup = do
>     atom "settings" $ do
>         watchdog <== Const (wdtPassword .|. wdtHold)
>         port1Dir <== Const (complement button)
>         port1Out <== Const 0
>         port1InterruptEnable <== Const button
>         port1InterruptFlags  <== Const 0
>     atom "poweroff" $ action (\_ -> "_BIS_SR(LPM0_bits|GIE)") []
> 
> isr = do
>     incr port1Out
>     port1InterruptFlags <== Const 0
> 
> main = mspCompile "g2231" $ mspProgram {
>     setupFn  = Just setup,
>     port1ISR = Just isr
>  }

 [LHS]: http://www.haskell.org/haskellwiki/Literate_programming
