# scheme->gba

A Scheme to GBA compiler, featuring very bad Scheme. Written on a flight from ORD to SFO using:
* _An Incremental Approach to Compiler Construction_ by Abdulaziz Ghuloum
* tonclib header files (from J Vijn's excellent TONC GBA reference)
* The Racket docs

Currently supports:
* Fixnums
* Immediate constants

Requires devKitPro (formerly devKitARM).

### gbc.rkt

Emits the assembly prelude (`prelude.s`) and the result of compiling `scheme.rkt` with the label `scheme_entry`.

### compile-program.rxt

Takes an `emit` function and a Scheme program and compiles it.

### driver.c

Calls `scheme_entry` and writes the result to IWRAM (address `0x03000000`), visible in the VisualBoyAdvance memory viewer.