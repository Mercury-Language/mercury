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

all:
	$(MMAKE) MMAKEFLAGS=$(PARALLEL) all

install:
	$(MMAKE) MMAKEFLAGS=$(PARALLEL) install

# `mmake clean' has a different meaning to the usual GNU standard `make clean':
# it removes the .c files, which makes bootstrapping impossible unless you
# have already installed a working Mercury compiler.
# Hence we don't just use `$(MMAKE) clean' here.

clean:
	-rm -f */*.o */*.pic_o */*.a */*.so */*.no */*.ql */*.pl
	-rm -f compiler/mercury_compile profiler/mercury_profile 
	-rm -f library/library.nu* library/library.sicstus*
	-rm -f library/sicstus_compile util/mdemangle util/mkinit
	cd boehm_gc; $(MMAKE_SUBDIR) clean

distclean: clean
	cd scripts; for file in *.in; do rm -f `basename $file .in`; done
	-rm -f Mmake.common runtime/conf.h
	-rm -f config.cache config.status config.log

maintainer-clean:
	$(MMAKE) clean

realclean:
	$(MMAKE) realclean

uninstall:
	$(MMAKE) uninstall

.DEFAULT:
	$(MMAKE) $@

#-----------------------------------------------------------------------------#
