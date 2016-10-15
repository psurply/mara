##
## Makefile for Mara in /
##
## Made by Pierre Surply
## <pierre.surply@gmail.com>
##
## Started on  Wed Oct 24 18:38:55 2012 Pierre Surply
## Last update Thu Aug 29 20:52:35 2013 Pierre Surply
##

VERSION		= 13
SUBDIRS		= compiler	\
		  samples

DIR_DIST	= mara_r$(VERSION)
TAR		= tar

FILES		= compiler	\
		  samples	\
		  stdlib	\
		  include	\
		  utils		\
		  doc		\
		  README.md	\
		  Makefile	\
		  CHANGES	\
		  COPYING

PREFIX		?= /usr/local
LIBCPATH	= $(PREFIX)/avr

all: compiler

clean: COMMAND=clean
clean: $(SUBDIRS)
	rm -rf $(DIR_DIST) *.tar.gz

$(SUBDIRS)::
	@$(MAKE) -C $@ $(COMMAND)

install: compiler
	@mkdir -p $(LIBCPATH)/lib/mara/
	@cp stdlib/* $(LIBCPATH)/lib/mara/
	@cp -R include/* $(LIBCPATH)/include/
	@cp compiler/marac $(PREFIX)/bin/

uninstall::
	@rm -rf $(PREFIX)/lib/mara/
	@rm -f $(PREFIX)/bin/marac

dist: clean
	rm -rf $(DIR_DIST)
	mkdir $(DIR_DIST)
	cp -R $(FILES) $(DIR_DIST)
	$(TAR) -zcvf $(DIR_DIST).tar.gz $(DIR_DIST)
