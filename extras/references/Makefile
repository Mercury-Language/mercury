#-----------------------------------------------------------------------------#
# vim: ts=8 sw=8 noexpandtab
#-----------------------------------------------------------------------------#
# Copyright (C) 2011 The University of Melbourne.
# Copyright (C) 2015, 2018 The Mercury team.
# This file is distributed under the terms specified in COPYING.LIB.
#-----------------------------------------------------------------------------#

# Build the global library using mmc --make.

TARGET=global

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
