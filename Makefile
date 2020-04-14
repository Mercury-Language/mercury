#-----------------------------------------------------------------------------#
# vim: ts=8 sw=8 noexpandtab
#-----------------------------------------------------------------------------#
# Copyright (C) 1995-1998, 2000, 2002, 2004-2005, 2010, 2012 The University of Melbourne.
# This file may only be copied under the terms of the GNU General
# Public Licence - see the file COPYING in the Mercury distribution.
#-----------------------------------------------------------------------------#
#
# This Makefile just invokes `mmake' to do the real work.
# Standard targets include `all', `install', and `clean'.
#
# If you want to do a parallel make, invoke make as
#	make PARALLEL=-j2
# or
#	make PARALLEL=-j2 install
# (`-j2' is for 2 jobs; you can use `-j3' for 3, etc.)
#
#-----------------------------------------------------------------------------#

SHELL=		/bin/sh
MMAKE=		MMAKE_DIR=`pwd`/scripts scripts/mmake
MMAKE_SUBDIR=	MMAKE_DIR=`pwd`/../scripts ../scripts/mmake $(PARALLEL)

# PARALLEL=-j2

#-----------------------------------------------------------------------------#

.PHONY: all
all:
	$(MMAKE) MMAKEFLAGS=$(PARALLEL) all

.PHONY: install
install:
	$(MMAKE) MMAKEFLAGS=$(PARALLEL) DESTDIR=$(DESTDIR) install

# `mmake clean' has a different meaning to the usual GNU standard `make clean':
# it removes the .c files, which makes bootstrapping impossible unless you
# have already installed a working Mercury compiler.
# Hence we don't just use `$(MMAKE) clean' here.

.PHONY: clean
clean:
	-rm -f */*.o */*.pic_o */*.a */*.so */*.dylib
	-rm -rf */Mercury/os */Mercury/pic_os */Mercury/libs
	-rm -f compiler/mercury_compile$(EXT_FOR_EXE)
	-rm -f profiler/mercury_profile$(EXT_FOR_EXE)
	-rm -f util/mdemangle$(EXT_FOR_EXE)
	-rm -f util/mfiltercc$(EXT_FOR_EXE)
	-rm -f util/mkinit$(EXT_FOR_EXE)
	cd boehm_gc; $(MMAKE_SUBDIR) clean

.PHONY: distclean
distclean: clean
	cd scripts; for file in *.in; do rm -f `basename $file .in`; done
	-rm -f Mmake.common runtime/mercury_conf.h
	-rm -f boehm_gc/mercury_boehm_gc_conf.h bindist/bindist.build_vars
	-rm -f config.cache config.status config.log

.PHONY: maintainer-clean
maintainer-clean:
	$(MMAKE) clean

.PHONY: realclean
realclean:
	$(MMAKE) realclean

.PHONY: uninstall
uninstall:
	$(MMAKE) uninstall

.DEFAULT:
	$(MMAKE) $@

#-----------------------------------------------------------------------------#
