# atom-msp430

Convenience functions for using [Atom][] with the MSP430 microcontroller family.
`atom-msp430` wraps common interactions with the 430 in Atom's type-safe and beautiful [Haskell][] DSL.
There is a Wiring-like setup/loop structure by default, or you can forgo the convenience functions to
compile whatever code structure fits your needs.

For a quick demonstration, have a read of [ledblink.lhs][].
More examples will come soon!
To see the current status and goals, have a look at the [roadmap][].

 [Atom]: https://github.com/tomahawkins/atom
 [Haskell]: http://www.haskell.org/
 [ledblink.lhs]: Language/Atom/MSP430/Examples/ledblink.lhs
 [roadmap]: https://github.com/eightyeight/atom-msp430/wiki/Roadmap

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
void initUART(void) {
    P1SEL = BIT1 + BIT2;        // P1.1 = RXD, P1.2=TXD
    P1SEL2 = BIT1 + BIT2;       // P1.1 = RXD, P1.2=TXD

    UCA0CTL1 |= UCSSEL_1;       // CLK = ACLK
    UCA0BR0 = 0x03;             // 32kHz/9600 = 3.41
    UCA0BR1 = 0x00;
    UCA0MCTL = UCBRS1 + UCBRS0; // Modulation UCBRSx = 3
    UCA0CTL1 &= ~UCSWRST;       // **Initialize USCI state machine**
    IE2 |= UCA0RXIE;            // Enable USCI_A0 RX interrupt
}
```

(Example by oPossum via [msp430launchpad.com][] with no malice intended.
Those guys taught me everything I know about the 430!)

 [msp430launchpad.com]: http://www.msp430launchpad.com/2012/06/using-printf.html
