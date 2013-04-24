# atom-msp430

Convenience functions for using [Atom][] with the MSP430 microcontroller family.
`atom-msp430` wraps common interactions with the 430 in Atom's type-safe and beautiful [Haskell][] DSL.
There is a Wiring-like setup/loop structure by default, or you can forgo the convenience functions to
compile whatever code structure fits your needs.

For a quick demonstration, have a read of [ledblink.lhs][].
More examples will come soon!

 [Atom]: https://github.com/tomahawkins/atom
 [Haskell]: http://www.haskell.org/
 [ledblink.lhs]: https://github.com/eightyeight/atom-msp430/blob/master/Language/Atom/MSP430/Examples/ledblink.lhs

## How?

 1. Download and install the [Haskell Platform][].
 2. Get Atom using Cabal-install: `cabal install atom`
 3. Clone this repository, then build and install using Cabal:

```
git clone git@github.com:eightyeight/atom-msp430
cd atom-msp430
cabal configure
cabal build
cabal install --user
```

 [Haskell Platform]: http://www.haskell.org/platform

## Why?

 1. Because Atom is cool, and Haskell is a great macro language.
 2. Because type-safety can mean more efficient learning as well as safer code.
 3. Because I'm sick of seeing code like this:

```c
void initTimer(void) {
   P1DIR |= BIT0; // set P1.0 (LED1) as output
   P1OUT |= BIT0; // P1.0 low
   CCTL0 = CCIE;  // CCR0 interrupt enabled
   CCR0 = 4096;   // 32kHz/8/4096 -> 1 sec
   TACTL = TASSEL_1 + ID_3 + MC_1; // ACLK, /8, upmode
}
```

(Example by oPossum via [msp430launchpad.com][] with no malice intended.
Those guys taught me everything I know about the 430!)

 [msp430launchpad.com]: http://www.msp430launchpad.com/2012/06/using-printf.html
