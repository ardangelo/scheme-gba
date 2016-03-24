#lang racket

; GBA-specific defines

(provide (all-defined-out))

(require "mem-defs.rkt")

(define ptr-iwram (ptr 'mem #x03000000))