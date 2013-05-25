LED blink
---------

_This is a [literate Haskell][LHS] file, which means GHC can read it as valid Haskell code.
Try `runhaskell ledblink.lhs` and see!_

Yes, the good old blinking LED.
This program will target the MSP430G2231 controller on a [LaunchPad][LP]
(which is where I get my assumptions about where LEDs are plugged in and so on).
Let's dive right in and get our top matter and imports out of the way.
For brevity, we'll just import everything `atom-msp430` has to offer (including all of Atom itself).

> module Main where
> import Language.Atom.MSP430

`atom-MSP430` has a default structure similar to Wiring or Arduino, with a `setup` function and a `loop` function.
We'll define `setup` first, because that's logical.
The first thing we need to do is disable (or 'hold') the watchdog timer.

> setup = do
>     watchdog $ do
>         set Password
>         set Hold

Next, we can use a direction register to set one pin in port 1 to output mode.
Here, 0 is the index of the pin that we want to set to output mode.

>     port1Dir $ do
>         set (Out 0)

Now for the `loop` function.
I'll assume you know the basics of [pulse width modulation][PWM] and digital IO.
What we're going to do is write the LED high every _p_ cycles, and write it low every _p_ cycles but offset by _p_ / 4.
This will keep the LED on a quarter of the time.
I've just chosen an arbitrary constant _p_ for you:

> p = 10000

And now the loop function itself.
We define two rules, `"led_high"` and `"led_low"` that fire with our desired period and phase.
Each rule uses the `port1Out` function to change the output of the IO port 1.
Note that here we use the `Pin` constructor instead of `Out` to refer to the 0th pin since we're talking about IO, not setting the direction.

> loop = do
>     period p $ atom "led_high" $ do
>         port1Out $ set (Pin 0)
>     period p $ phase (quot p 4) $ atom "led_low" $ do
>         port1Out $ clear (Pin 0)

And that's it!
Now we'll take advantage of one of `atom-msp430`'s convenience functions to compile our setup and loop functions.

> main = mspCompile "g2231" $ wiringProgram setup loop

This will automatically create four C library files (a header and code file for each function)
and a main file (`main.c`) that executes `setup` and `loop`.
It will look something like this:

    #include "setup.h"
    #include "loop.h"
    
    int main(void) {
        setup();
        while(1) loop();
        return 0;
    }

Compile with your choice of MSP430-compatible software (I use [IAR embedded kickstart][IAR]).
Once you download and run, you'll see the red LED blink slowly.
Hurrah!

 [LHS]: http://www.haskell.org/haskellwiki/Literate_programming
 [LP]: http://processors.wiki.ti.com/index.php/MSP430_LaunchPad_(MSP-EXP430G2)
 [PWM]: http://www.msp430launchpad.com/2010/07/timers-and-clocks-and-pwm-oh-my.html
 [IAR]: http://processors.wiki.ti.com/index.php/IAR_Embedded_Workbench_for_TI_MSP430
