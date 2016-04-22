PATH := $(DEVKITARM)/bin:$(PATH)

INCLUDE  := 
LIBPATHS := 

SPECS	:= -specs=gba_mb.specs
ARCH	:= -mthumb-interwork -mthumb

ASFLAGS	:= -mthumb-interwork
CFLAGS	:= $(ARCH) $(INCLUDE) -O2 -Wall -fno-strict-aliasing
LDFLAGS	:= $(ARCH) $(SPECS) $(LIBPATHS) $(LIBS) -Wl,-Map,$(PROJ).map

all: gbctest.gba

# compile scheme.rkt
scheme.s : scheme.rkt
	racket gbacompile.rkt -o scheme.s scheme.rkt

# compile the object files
main.o : main.c
	arm-none-eabi-gcc $(CFLAGS) -c main.c -o main.o


# # compile the main
# main.s : main.rkt
# 	racket gbacompile.rkt -o main.s main.rkt

# # assemble the main
# main.o : main.s
# 	arm-none-eabi-as $(ASFLAGS) -o main.o main.s

# assemble the output source
scheme.o : scheme.s
	arm-none-eabi-as $(ASFLAGS) -o scheme.o scheme.s

# link objects into an elf
gbctest.elf : main.o scheme.o
	arm-none-eabi-gcc main.o scheme.o $(LDFLAGS) -o gbctest.elf

# objcopy and fix the rom
gbctest.gba : gbctest.elf
	arm-none-eabi-objcopy -v -O binary gbctest.elf gbctest.gba
	gbafix gbctest.gba -tgbctest

clean :
	@rm -fv *.gba
	@rm -fv *.elf
	@rm -fv *.o
	@rm -fv scheme.s main.s .map
