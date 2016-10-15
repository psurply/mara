##
## Makefile for Mara
##
## Made by Pierre Surply
## <pierre.surply@gmail.com>
##
## Started on  Fri Jul 12 10:37:58 2013 Pierre Surply
## Last update Tue Aug 20 17:18:29 2013 Pierre Surply
##

MCU		= 328p
DEV		= /dev/ttyU0

MARAC		= ../compiler/marac
MFLAGS		= -mmcu atmega$(MCU) -verbose 5 -I ../stdlib/ -I ../include/

AVRDUDE		= avrdude
PARTNO		= m$(MCU)
PROG		= arduino -b 115200
AFLAGS		= -p $(PARTNO) -c $(PROG) -P $(DEV) -e

MAIN		= check
MAINHEX		= $(MAIN).hex

CSRC		= $(wildcard *.c)
SRC		= $(wildcard *.mr)

all: $(MAINHEX)

$(MAINHEX): $(SRC) $(CSRC)
	$(MARAC) $(MFLAGS) $(MAIN) $(CSRC)

upload: $(MAINHEX)
	$(AVRDUDE) $(AFLAGS) -U "flash:w:$(MAINHEX)"

checkall: upload
	minicom -w -D $(DEV)

checkhexa: upload
	minicom -H -w -D $(DEV)

clean::
	rm -f *~ *.hex *.o *.s
