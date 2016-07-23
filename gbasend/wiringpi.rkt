(module wiringpi racket/base

(provide (all-defined-out))

(require ffi/unsafe 
	 ffi/unsafe/define)

(define-ffi-definer define-wiringpi 
		    (ffi-lib "/usr/local/lib/libwiringPi.so"))

;; Foreign function interface to WiringPi (http://wiringpi.com)
;; why the camelCase function names? - easier to reference for those
;; familiar with wiringPi

;; I2C functions
(define-wiringpi wiringPiI2CSetup (_fun _int -> _int))
(define-wiringpi wiringPiI2CRead (_fun _int -> _int))
(define-wiringpi wiringPiI2CWrite (_fun _int _int -> _int))
(define-wiringpi wiringPiI2CWriteReg8 (_fun _int _int _int -> _int))
(define-wiringpi wiringPiI2CWriteReg16 (_fun _int _int _int -> _int))
(define-wiringpi wiringPiI2CReadReg8 (_fun _int _int -> _int))
(define-wiringpi wiringPiI2CReadReg16 (_fun _int _int -> _int))

;; SPI functions
(define-wiringpi wiringPiSPISetup (_fun _int _int -> _int))
(define-wiringpi wiringPiSPISetupMode (_fun _int _int _int -> _int))
(define-wiringpi wiringPiSPIDataRW (_fun _int _pointer _int -> _int))

(define-wiringpi wiringPiSetup (_fun -> _int))
(define-wiringpi wiringPiSetupGpio (_fun -> _int))
(define-wiringpi wiringPiSetupPhys (_fun -> _int))
(define-wiringpi wiringPiSetupSys (_fun -> _int))
(define-wiringpi pinMode (_fun _int _int -> _void))
(define-wiringpi pinModeAlt  (_fun _int _int -> _void))
(define-wiringpi pullUpDnControl (_fun _int _int -> _void))
(define-wiringpi digitalRead (_fun _int -> _int))
(define-wiringpi digitalWrite (_fun _int _int -> _void))
(define-wiringpi pwmWrite (_fun _int _int -> _void))

;; Raspberry Pi hardware-specific

(define-wiringpi piBoardRev (_fun -> _int))
(define-wiringpi pwmSetMode (_fun _int -> _void))
(define-wiringpi pwmSetRange (_fun _uint -> _void))
(define-wiringpi pwmSetClock (_fun _int -> _void))

;; Software PWM

(define-wiringpi softPwmCreate (_fun _int _int _int -> _int))
(define-wiringpi softPwmWrite  (_fun _int _int -> _void))
(define-wiringpi softPwmStop   (_fun _int -> _void))


(define WPI_MODE_PINS         0)
(define WPI_MODE_GPIO         1)
(define WPI_MODE_GPIO_SYS     2)
(define WPI_MODE_PHYS         3)
(define WPI_MODE_PIFACE       4)
(define WPI_MODE_UNINITIALISED -1)

;; pin modes

(define	INPUT       0)
(define	OUTPUT	    1)
(define	PWM_OUTPUT  2)
(define	GPIO_CLOCK  3)
(define	SOFT_PWM_OUTPUT	 4)
(define SOFT_TONE_OUTPUT 5)

(define	LOW 0)
(define	HIGH 1)

;; pull up/down/none

(define PUD_OFF         0)
(define PUD_DOWN        1)
(define PUD_UP          2)

;; PWM

(define	PWM_MODE_MS	0)
(define	PWM_MODE_BAL	1)

;; Interrupt levels

(define	INT_EDGE_SETUP	 0)
(define	INT_EDGE_FALLING 1)
(define	INT_EDGE_RISING	 2)
(define	INT_EDGE_BOTH	 3)

;; Pi model types

(define	PI_MODEL_A	0)
(define	PI_MODEL_B	1)
(define	PI_MODEL_CM	2)

)


