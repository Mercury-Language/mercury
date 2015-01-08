#-----------------------------------------------------------------------------#
# vim: ts=8 sw=8 noexpandtab
#-----------------------------------------------------------------------------#
# Copyright (C) 2002-2007 The University of Melbourne
# Ralph Becket <rafe@cs.mu.oz.au>
#
# To build do:
#
# $ make
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

all: test_regex

%: %.m
	mmc --make --mld $(EXTRA_LIB_DIR) $(EXTRA_LIBRARIES_FLAGS) $@

check: test_regex
	./test_regex < test_regex.in > test_regex.res
	diff -u test_regex.exp test_regex.res && echo "Passed" || echo "Failed"
