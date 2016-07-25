# scheme->gba

A Scheme compiler targeting ARM assembly and Game Boy Advance. Started on a flight from ORD to SFO using:
* _An Incremental Approach to Compiler Construction_ by Abdulaziz Ghuloum
* tonclib header files (from J Vijn's excellent TONC GBA reference)
* The Racket docs

Currently supports:
* Fixnums
* Immediate constants
* Primitive procedures: `add1`, `sub1`, `integer->char`, `char->integer`, `null?`, `zero?`, `not`, `integer?`, `boolean?`, `+`, `-`, `*`, `let` (immediate constants only), `if`, `cons`, `cons?`, `car`, `cdr`, `make-vector`, `vector?`, `vector-ref`, `vector-set!`, `make-string`, `string?`, `string-ref`, `string-set!`
* `lambda`, `funcall`, `tailcall`

Requires devKitPro (formerly devKitARM).

## Compiler components

### gbacompile.rkt

Emits the assembly prelude (`prelude.s`) and the result of compiling `scheme.rkt` with the label `scheme_entry` (specify a different source file with the `-o` flag).

### compile-program.rkt

Takes an `emit` function and a Scheme program and compiles it.

### gba.rkt, mem-defs.rkt

Contains GBA/memory-specific constants and functions

### driver.c

Calls `scheme_entry` and writes the result to WRAM (address `0x02000000`), visible in the VisualBoyAdvance memory viewer.

## Utilities

### gbasend.rkt

Located in the `gbasend` folder. Takes a GBA multiboot ROM as an argument and sends it over Link Cable via Raspberry Pi GPIO pins. Requires a Raspberry Pi and GBA Link Cable. I used a $2 cable and jumper wire from a cheap Chinese electronics site; only one end of the Link Cable had the necessary pins for multiboot.

Wiring:
`RPI GPIO    /  GBA LINK IO`
`PIN 19/MOSI -> PIN 3/SI`
`PIN 21/MISO -> PIN 2/SO`
`PIN 23/SCLK -> PIN 5/SC`
`PIN 25/GND  -> PIN 6/GND`

Cable pinout (via [HardwareBook](http://www.hardwarebook.info/Game_Boy_Link#Pinout)):
` _________  `
`/ 6  4  2 \ `
`\_5_ 3 _1_/  (at cable)`
`    '-'`