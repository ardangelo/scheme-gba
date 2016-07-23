#lang racket

;; A big thank you to:
;;  `akkera102` whose C code I ported to Racket (https://github.com/akkera102/gba_01_multiboot)
;;  `stevelloyd` for wiringpi bindings (https://github.com/stevelloyd/wiringpi-racket)

(require ffi/unsafe)
(require "wiringpi.rkt")
(require racket/cmdline)
(require racket/fixnum)

;; SPI config
(define spi-clock 100000)
(define spi-channel 0)

;; magic numbers
(define req-recognition #x00006202)
(define got-recognition #x72026202)
(define ack-recognition #x00006102)

(define head-complete #x00006200)
(define send-palette #x000063d1)

(define enc-c #x0000c387)

(define req-crc #x00000065)
(define got-crc #x00750065)
(define ack-crc #x00000066)

;; bitwise functions

(define (get-byte w n)
  (let* (
         [shamt (* n 8)]
         [mask (arithmetic-shift #xff shamt)])
    (arithmetic-shift (bitwise-and w mask) (- shamt))))

(define (shift32 w n)
  (bitwise-and
   (arithmetic-shift w n)
   (- (arithmetic-shift 1 32) 1)))

(define (bytes->integer bytes)
  (let ([bytelist (bytes->list bytes)])
    (foldl (lambda (x acc)
             (bitwise-ior x (arithmetic-shift acc 8)))
           0 bytelist)))


;; SPI functions

(define (write-spi w buf)

  ; set up buffer
  (for ([i (in-range 4)])
    (array-set! buf (- 3 i) (get-byte w i)))
  
  ; write out to spi pins
  (wiringPiSPIDataRW spi-channel (array-ptr buf) 4)
  
  (+ 
   (arithmetic-shift (array-ref buf 0) 24)
   (arithmetic-shift (array-ref buf 1) 16)
   (arithmetic-shift (array-ref buf 2) 8)
   (array-ref buf 3)))
(define (write-spi! w buf)
  (write-spi w buf)
  (void))

;; parse arguments

(define rom-to-send
  (command-line 
   #:args (rom)
   rom))
(define rom-bytes (port->bytes (open-input-file rom-to-send #:mode 'binary)))

(define buf-type (_array _uint8 4))
(define buf (ptr-ref (malloc buf-type) buf-type 0))

;; make sure rom is small enough

(cond
  [(< #x40000 (bitwise-and (+ (bytes-length rom-bytes) #x0f) #xfffffff0))
   (error "Error: ROM must be smaller than 256kB")])

;; set up transfer

(define spi-fd (wiringPiSPISetupMode spi-channel spi-clock 3))

(displayln "searching for GBA...")
(let loop ()
  (when (not (= 
              (write-spi req-recognition buf)
              got-recognition))
    
    (sleep .0001)
    (loop)))
(displayln "GBA found!")

(write-spi! ack-recognition buf)
(displayln "recognition acknowledged")

(displayln "sending header...")
(define fcnt 0)
(for ([i (in-range 0 (+ (* 2 #x5f) 1) 2)])
  (write-spi! (bitwise-ior
               (bytes-ref rom-bytes i)
               (shift32 (bytes-ref rom-bytes (+ i 1)) 8)) buf)
  (set! fcnt (+ 2 fcnt)))

(write-spi! head-complete buf)
(displayln "finished sending header")
(write-spi! req-recognition buf)
(displayln "resend recognition request")

(write-spi! send-palette buf)
(define handshake-data (write-spi send-palette buf))

(define m (+ (shift32 (bitwise-and handshake-data #x00ff0000) -8) #xffff00d1))
(define h (+ (shift32 (bitwise-and handshake-data #x00ff0000) -16) #xf))

(displayln "send handshake data")
(write-spi! (bitwise-ior (bitwise-and (+ (shift32 handshake-data -16) #xf) #xff) #x00006400) buf)
(displayln "send rom length")
(define seed (write-spi (quotient (- (bytes-length rom-bytes) #x190) 4) buf))
(displayln (format "got seed 0x~x" seed))

(define f (bitwise-ior (+ (shift32 (bitwise-and seed #x00ff0000) -8) h) #xffff0000))
(define c #x0000c387)

(displayln "finished setting up encryption, start sending rom")

(let loop ()
  (when (< fcnt (bytes-length rom-bytes))
    (let ([word (list->bytes (reverse (bytes->list (subbytes rom-bytes fcnt (+ fcnt 4)))))])
      (for ([bit (in-range 32)])
        (if (zero? (bitwise-and (bitwise-xor c (shift32 (bytes->integer word) (- bit))) #x01))
            (set! c (shift32 c -1))
            (set! c (bitwise-xor (shift32 c -1) #x0000c37b))))
      (set! m (bitwise-and (+ (* #x6f646573 m) 1) #xffffffff))
      (write-spi! (bitwise-xor (bytes->integer word) (+ (bitwise-not (+ #x02000000 fcnt)) 1) m #x43202f2f) buf)
      (set! fcnt (+ fcnt 4)))
    (loop)))

(for ([bit (in-range 32)])
  (if (zero? (bitwise-and (bitwise-xor c (shift32 f (- bit))) #x01))
      (set! c (shift32 c -1))
      (set! c (bitwise-xor (shift32 c -1) #x0000c37b))))

(displayln "waiting for crc...")
(let loop ()
  (when (not (= 
              (write-spi req-crc buf)
              got-crc))
    
    (sleep .0001)
    (loop)))
(write-spi! ack-crc buf)
(displayln "acknowledged crc")

(write-spi! c buf)

(displayln "sent our crc, multiboot complete")
