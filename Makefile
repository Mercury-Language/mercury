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
	$(MMAKE) MMAKEFLAGS=$(PARALLEL) all 2>&1 | tee make_all.log

.PHONY: install
install:
	$(MMAKE) MMAKEFLAGS=$(PARALLEL) install 2>&1 | tee make_install.log

# `mmake clean' has a different meaning to the usual GNU standard `make clean':
# it removes the .c files, which makes bootstrapping impossible unless you
# have already installed a working Mercury compiler.
# Hence we don't just use `$(MMAKE) clean' here.

.PHONY: clean
clean:
	-rm -f */*.o */*.pic_o */*.a */*.so */*.no */*.ql */*.pl
	-rm -f compiler/mercury_compile profiler/mercury_profile 
	-rm -f library/library.nu* library/library.sicstus*
	-rm -f library/sicstus_compile util/mdemangle util/mkinit
	cd boehm_gc; $(MMAKE_SUBDIR) clean

.PHONY: distclean
distclean: clean
	cd scripts; for file in *.in; do rm -f `basename $file .in`; done
	-rm -f Mmake.common runtime/mercury_conf.h bindist/bindist.build_vars
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
