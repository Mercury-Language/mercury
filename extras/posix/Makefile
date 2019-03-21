#-----------------------------------------------------------------------------#
# vim: ts=8 sw=8 noexpandtab
#-----------------------------------------------------------------------------#
# Copyright (C) 2011 The University of Melbourne.
# Copyright (C) 2015, 2018-2019 The Mercury team.
# This file is distributed under the terms specified in COPYING.LIB.
#-----------------------------------------------------------------------------#

# Build the posix library using mmc --make.

TARGET = posix
MMC = mmc

.PHONY: build
build: posix_workarounds.o
	$(MMC) --make lib$(TARGET)

.PHONY: install
install: posix_workarounds.o
	$(MMC) --make lib$(TARGET).install

posix_workarounds.o: posix_workarounds.c

.PHONY: realclean
realclean:
	cd samples && $(MAKE) realclean
	$(MMC) --make $(TARGET).realclean
	/bin/rm -f posix_workarounds.o
	/bin/rm -rf Mercury
