##
## Makefile for Mara in /compiler
##
## Made by Pierre Surply
## <pierre.surply@gmail.com>
##
## Started on  Fri Nov  9 15:58:04 2012 Pierre Surply
## Last update Wed Jul 17 17:28:24 2013 Pierre Surply
##

OCAMLBUILD	= ocamlbuild
BUILDTYPE	?= native

ifeq ($(BUILDTYPE),byte)
  LIBS		= unix.cma,str.cma
else
  LIBS		= unix.cmxa,str.cmxa
endif

BUILDFLAGS	= -lflags $(LIBS)		\
		  -I parser			\
		  -I semantic			\
		  -I backend			\
		  -I fsm

EXEC		= marac

MAIN		= main.$(BUILDTYPE)
SRC		= $(wildcard *.ml)		\
		  $(wildcard *.mli)		\
		  $(wildcard parser/*.ml)	\
		  $(wildcard parser/*.mli)	\
		  $(wildcard parser/*.mll)	\
		  $(wildcard parser/*.mly)	\
		  $(wildcard semantic/*.ml)	\
		  $(wildcard semantic/*.mli)	\
		  $(wildcard asm/*.ml)		\
		  $(wildcard asm/*.mli)

all: $(EXEC)

$(EXEC): $(MAIN)
	@mv _build/$^ $@
	@rm $(MAIN)

$(MAIN): $(SRC)
	@$(OCAMLBUILD) $(BUILDFLAGS) $@

clean::
	rm -f *~ $(EXEC)
	@$(OCAMLBUILD) -clean
