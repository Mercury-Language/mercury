#-----------------------------------------------------------------------------#
# vim: ts=8 sw=8 noexpandtab
#-----------------------------------------------------------------------------#
# Copyright (C) 2017 The Mercury team.
# This file may only be copied under the terms of the GNU Library General
# Public License - see the file COPYING.LIB in the Mercury distribution.
#-----------------------------------------------------------------------------#

# Build the old term parser library using mmc --make.

TARGET=old_term_parser

.PHONY: build
build:
	mmc --make lib$(TARGET)

.PHONY: install
install:
	mmc --make lib$(TARGET).install

.PHONY: realclean
realclean:
	mmc --make $(TARGET).realclean
	/bin/rm -rf Mercury
