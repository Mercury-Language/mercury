#-----------------------------------------------------------------------------#
# vim: ts=8 sw=8 noexpandtab
#-----------------------------------------------------------------------------#
# Copyright (C) 2017-2018 The Mercury team.
# This file is distributed under the terms specified in COPYING.LIB.
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
