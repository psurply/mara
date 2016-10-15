##
## Makefile for Mara
##
## Made by Pierre Surply
## <pierre.surply@gmail.com>
##
## Started on  Fri Jul 12 10:37:58 2013 Pierre Surply
## Last update Wed Aug 21 19:08:18 2013 Pierre Surply
##

MCU		= 328p
DEV		= /dev/ttyU0

MARAC		= ../compiler/marac
MFLAGS		= -mmcu atmega$(MCU) -verbose 1 -I ../stdlib/ -I ../include

AVRDUDE		= avrdude
PARTNO		= m$(MCU)
PROG		= arduino -b 115200
AFLAGS		= -p $(PARTNO) -c $(PROG) -P $(DEV) -e

SAMPLES		= blink.mr		\
		  ext_int.mr		\
		  hello.mr		\
		  echo.mr		\
		  fibo.mr		\
		  analog.mr		\
		  watchdog.mr		\
		  sleep.mr		\
		  class.mr		\
		  fsm.mr		\
		  99bob.mr

HEX		= $(SAMPLES:.mr=.hex)

all: $(HEX)
	$(MARAC) $(MFLAGS) c_interfacing.mr mara_interfacing.c

%.hex: %.mr
	$(MARAC) $(MFLAGS) $^

upload: $(MAINHEX)
	$(AVRDUDE) $(AFLAGS) -U "flash:w:$(MAINHEX)"

clean::
	rm -f *~ *.hex *.o
