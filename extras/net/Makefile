#-----------------------------------------------------------------------------#
# vim: ts=8 sw=8 noexpandtab
#-----------------------------------------------------------------------------#
# Copyright (C) 2014 The Mercury Team
# This file may only be copied under the terms of the GNU Library General
# Public Licence - see the file COPYING in the Mercury distribution.
#-----------------------------------------------------------------------------#

MMC=mmc
MTAGS=mtags
MCFLAGS=-O3 --intermodule-optimization --use-grade-subdirs --grade hlc.gc

MERCURY_FILES=$(wildcard *.m)

all: libnet.so echo test_lookups

libnet.so: Mercury.modules
	$(MMC) $(MCFLAGS) --make libnet

echo: Mercury.modules
	$(MMC) $(MCFLAGS) --make echo

test_lookups: Mercury.modules
	$(MMC) $(MCFLAGS) --make test_lookups

Mercury.modules: $(MERCURY_FILES)
	$(MMC) -f $(MERCURY_FILES)

tags: $(MERCURY_FILES)
	$(MTAGS) $(MERCURY_FILES)

.PHONY: clean
clean:
	rm -rf Mercury *.mh *.err \
		echo test_lookups libnet.so libnet.a \
		net.init \
		tags
