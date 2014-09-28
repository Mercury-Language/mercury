# vim: noet ts=4 sw=4
#-----------------------------------------------------------------------------#
# Copyright (C) 2014 The Mercury Team
# This file may only be copied under the terms of the GNU Library General
# Public Licence - see the file COPYING in the Mercury distribution.
#-----------------------------------------------------------------------------#

MMC=mmc
MTAGS=mtags
MCFLAGS=-O3 --intermodule-optimization --use-grade-subdirs --grade hlc.gc


MERCURY_FILES=$(wildcard *.m)

all : libnet.so echo

libnet.so : $(MERCURY_FILES)
	$(MMC) $(MCFLAGS) --make libnet

echo : $(MERCURY_FILES)
	$(MMC) $(MCFLAGS) --make echo

tags : $(MERCURY_FILES)
	$(MTAGS) $(MERCURY_FILES)
