LED blink
---------

_This is a [literate Haskell][LHS] file, which means GHC can read it as valid Haskell code.
Try `runhaskell ledblink.lhs` and see!_

Yes, the good old blinking LED.
This program will target the MSP430G2231 controller on a [LaunchPad][LP]
(which is where I get my assumptions about where LEDs are plugged in and so on).
Let's dive right in and get our top matter and imports out of the way.
For brevity, we'll just import everything `atom-msp430` has to offer (including all of `atom` itself).

> module Main where
> import Language.Atom.MSP430

`atom-MSP430` has a default structure similar to Wiring or Arduino, with a `setup` function and a `loop` function.
We'll define `setup` first, because that's logical.
The first thing we need to do is disable (or 'hold') the watchdog timer.

> setup = do
>     watchdog $ do
>         set Password
>         set Hold

And now, we'll set the `P1DIR` register to let us output values to our LED.
**Note: this demonstrates using the `atom` API to access a register.
`atom-msp430` doesn't wrap this in MSP430-specific code yet.
It will soon.**

>     let p1dir = word16' "P1DIR"
>     p1dir <== 1

Now for the `loop` function.
I'll assume you know the basics of [pulse width modulation][PWM] and digital IO.
What we're going to do is write the LED high every _p_ cycles, and write it low every _p_ cycles but offset by _p_ / 4.
This will keep the LED on a quarter of the time.
I've just chosen an arbitrary constant _p_ for you:

> p = 10000

And now the loop function itself.
Again, I'll use `atom`'s `word16'` function to get a reference to the `P1OUT` register of the G2231.
Then, we can define two rules, `"led_high"` and `"led_low"` that fire with our desired period and phase.

> loop = do
>     let p1out = word16' "P1OUT"
>     period p $ atom "led_high" $ do
>         p1out <== 1
>     period p $ phase (quot p 4) $ atom "led_low" $ do
>         p1out <== 0

And that's it!
Now we'll take advantage of one of `atom-msp430`'s convenience functions to compile our setup and loop functions.

> main = compileMSP430 setup loop

This will automatically create four C library files, a header and code file for each function.
Now, that's unfortunately not _all_ you need to get up and running.
This code has generated our library files, but we still need a main function to call these functions.
For example, I use something like this:

```c
int main(void) {
	setup();
	while(1) loop();
}
```

Don't forget to include `setup.h` and `loop.h`.
This compiles with [IAR embedded workbench][IAR], my IDE of choice for LaunchPad development.
Once you download and run, you'll see the red LED blink slowly.
Hurrah!

 [LHS]: http://www.haskell.org/haskellwiki/Literate_programming
 [LP]: http://processors.wiki.ti.com/index.php/MSP430_LaunchPad_(MSP-EXP430G2)
 [PWM]: http://www.msp430launchpad.com/2010/07/timers-and-clocks-and-pwm-oh-my.html
 [IAR]: http://processors.wiki.ti.com/index.php/IAR_Embedded_Workbench_for_TI_MSP430
