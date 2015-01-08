#-----------------------------------------------------------------------------#
# vim: ts=8 sw=8 noexpandtab
#-----------------------------------------------------------------------------#
# Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
# Copyright (C) 2002-2007, 2011 The University of Melbourne

# To build, do the following:
#
# $ make all
# $ make install
#

# Omit this line if you want to install in the standard location.
# Edit this line if you want to install the library elsewhere.
# A directory $(INSTALL_PREFIX)/lib/mercury will be created, if
# necessary, and everything put there.
#
#INSTALL_PREFIX := $(INSTALL_PREFIX)/extras
INSTALL_PREFIX = .

# Omit this line if you want to install the default grades.
# Edit this line if you want to install with different grades.
#
#LIBGRADES = asm_fast.gc hlc.gc asm_fast.gc.tr.debug

# Any application using these libraries will also need to pass the following
# options to mmc:
#
# --mld $(INSTALL_PREFIX)/lib/mercury --ml lex --ml regex

MAIN_TARGET = all

.PHONY: all install check realclean

all: liblex.compile libregex.compile

install: liblex.install libregex.install

ifdef LIBGRADES
LIBGRADES_FLAGS=--no-libgrade $(LIBGRADES:%=--libgrade %)
else
LIBGRADES_FLAGS=
endif

%.compile:
	mmc --make $(LIBGRADES_FLAGS) $*
%.install:
	mmc --make $(LIBGRADES_FLAGS) --install-prefix $(INSTALL_PREFIX) $@

%.realclean:
	mmc --make lex.realclean
	mmc --make regex.realclean
	/bin/rm -rf Mercury

check: install
	(cd tests; mmake check)
