# Mara Programming Language

## What is Mara ?

*Mara* is a programming language dedicated to [8bit ATMEL AVR][4]
microcontrollers with a focus on simplicity and productivity.

It provides to *AVR* architecture some features that high-level programming
languages have introduced on 32/64 bits processors, such as classes and
type inference.

Thus, the goal of this project is to simplify the programming of these
microcontrollers for all those who do not wish to use (or learn) the *C*
or *assembly language*.

Some features of Mara :

  - Simple and structured syntax
  - Type inference
  - Classes and objects
  - Explicit error messages
  - Standard library containing usual data structures
  - Inline assembler
  - Finite-state machines

*Mara* is also compatible with [Arduino][5] boards based on AVR.

## Example

    ##
    ## Program tested on an Atmega328p
    ## with a LED connected on PB5 pin.
    ##
    ## CPU frequency : 16 Mhz
    ##

    # Include standard library
    using string

    # Function declaration
    func print(s)
      for i <- 0 to s.length() - 1 do
        waitfor not (@UCSR0A bitand UDRE0)
        @UDR0 <- s[i]
      done
    end

    # Setting up the microcontroller
    setup
      # Variables declaration
      var buff <- new string(256)
      var tick <- true
      var suffix <- none
      var nsuffix <- none

      # Set PB5 as an output
      @DDRB <- DDB5

      # Set timer1
      @TCCR1B <- CS11 CS10
      @TIMSK1 <- TOIE1

      # Set UART (115200 bauds 8N1)
      @UBRR0H:UBRR0L <- 16
      @UCSR0A <- U2X0
      @UCSR0B <- TXEN0
      @UCSR0C <- UCSZ01 UCSZ00

      # Enable interrupts
      sei
    end

    # Timer1 overflow ISR
    interrupt TIMER1_OVF
      @PORTB ^^ PORTB5
      tick <- false
    end

    # Repeat forever
    loop
      suffix <- "s"
      for i <- 99 downto 1 do
        nsuffix <- i - 2 and i - 1 ? "s" : ""
        buff.format("%d bottle%s of beer on the wall, %d bottle%s of beer.\n\r"\
                    "Take one down and pass it around, %d bottle%s of beer "\
                    "on the wall.\n\n\r", \
                    {i; suffix; i; suffix; i - 1; nsuffix})
        print(buff)
        waitfor tick
        tick <- true
        suffix <- nsuffix
      done

      buff.write("No more bottles of beer on the wall, no more bottles of beer"\
                 ".\n\rGo to the store and buy some more, 99 bottles of beer" \
                 "on the wall.\n\n\r")
      print(buff)
      waitfor tick
      tick <- true
    end

## Dependencies

* [OCaml >= 3.12][1]
* [Menhir][6]
* OCamllex
* [avr-binutils][2]
* [avr-gcc][2]
* [avr-libc][2]

### Recommended

* [avrdude][3] (driver program for AVR programmer)

### Edit Mara code with Emacs

First, copy the file `utils/mara-mode.el` in your `~/.emacs.d/` directory.

Then, add the following lines to your `~/.emacs` file :

    (autoload 'mara-mode "~/.emacs.d/mara-mode.el" "" t)
    (setq auto-mode-alist (cons '("\\.mr\\w?" . mara-mode) auto-mode-alist))

## Installation

    $ make
    # make install PREFIX=/usr/local

## Usage

    usage : marac <options> <files>

      -mmcu <mcu>          Targeted microcontroller model (default: atmega8)
      -S                   Compile only; Do not assemble or link
      -c                   Compile and assemble, but do not link
      -I <dir>             Include folder
      -v                   Print compiler version and location of standard library and exit
      -verbose <verbosity> Set verbosity level (default: 2)
            0: quiet
            1: only print error messages
            2: print processing step dynamically
            3: print processing step
            4: print calls to external program
            5: print infered types
      -ccopt <opt>         Pass option <opt> to the C compiler and linker
      -cclib <opt>         Pass option <opt> to the C linker
      -help                Display this list of options
      --help               Display this list of options

[1]: http://caml.inria.fr/ocaml/index.en.html
[2]: http://www.nongnu.org/avr-libc/
[3]: http://savannah.nongnu.org/projects/avrdude
[4]: http://www.atmel.com/products/microcontrollers/avr/default.aspx
[5]: http://arduino.cc/
[6]: http://gallium.inria.fr/~fpottier/menhir/
