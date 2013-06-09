Interrupt
---------

_This is a [literate Haskell][LHS] file, which means GHC can read it as valid Haskell code.
Try `runhaskell interrupt.lhs` and see!_

This program is an example of using Timer A interrupts, in this case just to blink the red LED.
You can obviously add your own ISR code that does more interesting things!

> module Main where
> import Language.Atom.MSP430

In our setup function, the first thing we will do, as always, is disable the watchdog timer.
Then we'll set the direction of IO port 1's pins.
It's good practise to set unused pins to output mode, to prevent 'floating' inputs from affecting the MCU's operation,
so we'll do that as well, and then set pin 0's output high.

> setup = do
>     watchdog <== Const (wdtPassword .|. wdtHold)
>     port1Dir <== Const (complement 0)
>     port1Out <== Const 0x0001

Now we can configure the Timer A module.
The timer has several counting modes - we will use 'up' mode, where the timer starts at 0,
counts up to whatever value is in `TACCR0`, then resets to 0.
We also need to 'source' the timer ticks from the `ACLK`, one of the MSP430's internal oscillators (clocks).

>     timerAControl <== Const (taUpMode .|. taSourceACLK)

Now, we need to configure the timer to interrupt when it reaches the value in the capture/compare register.
This is enabled in the `TACCNTL` registers:

>     timerACCC0 <== Const taCCRInterrupt

Then we need to add some suitable value for the timer to count up to.
4096 gives a nice delay, so we'll put it in the capture/compare register:

>     timerACCR0 <== Const 4096

And, finally, we need to enable the global interrupt flag so the Timer A interrupts are received at all.

>     call "__enable_interrupt"

When the Timer A interrupt happens, we'll increment the value in port 1's output register.
This will have the effect of toggling the red LED (since the red LED is on when `port1Out` is odd).
Eventually the gren LED will turn on, but not for a long time!

> isr = do
>     incr port1Out

Since there is only one command in our ISR, we could have skipped the `do` notation,
which is used in Haskell to glue multiple commands together.
That would make our ISR look like:

    isr = incr port1Out

Finally, we'll compile the program using the `mspProgram` defaults with some overridden values instead of the `wiringProgram` convenience function.
Here we provide the compiler with the functions we use for setup, and the Timer A ISR.

> main = mspCompile "g2231" $ mspProgram {
>     setupFn = Just setup,
>     timerAISR = Just isr
>  }

As usual, this will generate `setup.h` and `.c` files, as well as `timerAISR.h` and `.c`,
and a `main.c` which calls `setup()` and assigns the Timer A interrupt function to the appropriate vector.

 [LHS]: http://www.haskell.org/haskellwiki/Literate_programming
