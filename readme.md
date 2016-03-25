# scheme->gba

A Scheme compiler targeting ARM assembly and Game Boy Advance. Started on a flight from ORD to SFO using:
* _An Incremental Approach to Compiler Construction_ by Abdulaziz Ghuloum
* tonclib header files (from J Vijn's excellent TONC GBA reference)
* The Racket docs

Currently supports:
* Fixnums
* Immediate constants
* Primitive procedures: `add1`, `sub1`, `integer->char`, `char->integer`, `null?`, `zero?`, `not`, `integer?`, `boolean?`, `+`, `-`, `*`, `let` (immediate constants only), `if`, `cons`

Requires devKitPro (formerly devKitARM).

### gbacompile.rkt

Emits the assembly prelude (`prelude.s`) and the result of compiling `scheme.rkt` with the label `scheme_entry` (specify a different source file with the `-o` flag).

### compile-program.rkt

Takes an `emit` function and a Scheme program and compiles it.

### gba.rkt, mem-defs.rkt

Contains GBA/memory-specific constants and functions

### driver.c

Calls `scheme_entry` and writes the result to WRAM (address `0x02000000`), visible in the VisualBoyAdvance memory viewer.