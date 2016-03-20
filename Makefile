PATH := $(DEVKITARM)/bin:$(PATH)

INCLUDE  := 
LIBPATHS := 

SPECS	:= -specs=gba.specs
ARCH	:= -mthumb-interwork -mthumb

ASFLAGS	:= -mthumb-interwork
CFLAGS	:= $(ARCH) $(INCLUDE) -O2 -Wall -fno-strict-aliasing
LDFLAGS	:= $(ARCH) $(SPECS) $(LIBPATHS) $(LIBS) -Wl,-Map,$(PROJ).map

all: gbctest.gba

# compile the object files
driver.o : driver.c
	arm-none-eabi-gcc $(CFLAGS) -c driver.c -o driver.o

# assemble the output source
scheme.o : scheme.s
	arm-none-eabi-as $(ASFLAGS) -o scheme.o scheme.s

# link objects into an elf
gbctest.elf : driver.o scheme.o
	arm-none-eabi-gcc driver.o scheme.o $(LDFLAGS) -o gbctest.elf

# objcopy and fix the rom
gbctest.gba : gbctest.elf
	arm-none-eabi-objcopy -v -O binary gbctest.elf gbctest.gba
	gbafix gbctest.gba -tgbctest

clean :
	@rm -fv *.gba
	@rm -fv *.elf
	@rm -fv *.o