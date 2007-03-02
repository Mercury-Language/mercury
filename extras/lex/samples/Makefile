# Copyright (C) 2001-2007 The University of Melbourne
# Ralph Becket <rafe@cs.mu.oz.au>
#
# To build do:
#
# $ make
#
# The targets are called lex_demo and regex_demo.
#
# Ensure you have built and installed the lex and regex libraries.
# Change the following line as appropriate if you installed them
# elsewhere:
#
#EXTRA_LIB_DIR := $(INSTALL_PREFIX)/lib/mercury
EXTRA_LIB_DIR := ../lib/mercury

EXTRA_LIBRARIES = lex regex
EXTRA_LIBRARIES_FLAGS = $(EXTRA_LIBRARIES:%= --ml %)

MAIN_TARGET = all

.PHONY: all check

all: lex_demo regex_demo

%: %.m
	mmc --make --mld $(EXTRA_LIB_DIR) $(EXTRA_LIBRARIES_FLAGS) $@

