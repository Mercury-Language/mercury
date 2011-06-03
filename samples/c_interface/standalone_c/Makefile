# This directory contains an example of how to create and use a stand-alone
# interface.  Stand-alone interfaces allow exported Mercury procedures to be
# called from "foreign" applications, that is applications whose entry point
# is something other than a Mercury main/2 predicate.  (By "exported" Mercury
# procedure we mean one that is the subject of a pragma foreign_export
# declaration.)
#
# A stand-alone interface consists of an object / header file pair.  These
# define a pair of functions whose respective tasks are to initialise and
# shutdown the Mercury runtime plus a given set of Mercury libraries that the
# foreign application may wish to use.  It is important to initialise the
# Mercury runtime before calling any Mercury procedures.  The header file
# created as part of the stand-alone interface is compatible with either C or
# C++.
#
# Stand-alone interfaces are created by invoking the compiler with the
# `--generate-standalone-interface' option.  The Mercury libraries that the
# foreign application may wish to use are specified via the usual mechanisms,
# e.g. the `--ml' and `--mld' options.  The Mercury standard library is always
# included amongst the set of libraries.
#
# In this example there is a small foreign application written in C contained
# in the file c_main.c.  This application calls some Mercury procedures
# defined in the Mercury library `mercury_lib' (which is contained in the file
# mercury_lib.m).  The program also manipulates the value of a mutable defined
# in this library.
#
# To build the application we first compile `mercury_lib'.  For this example
# we don't bother installing it since that would just lead to the command
# lines being unwieldy.  We then build the stand-alone interface, which in
# this example is called mercury_lib_int.  Finally we compile c_main.c and
# link them all together.  Specific details concerning each step in the build
# process are discussed below.  See c_main.c for details of how to invoke the
# stand-alone interface from C or C++ code.

MMC = mmc

all: c_main

# This variables holds the options required to set the grade.
# By default this is empty so the example will build in the default grade.
# To try, for example, a deep profiling grade, set it to:
#
#    GRADEOPT=--grade asm_fast.gc.profdeep
#
GRADEOPT=

# By default we link our application statically against the Mercury libraries.
# If you wish to use the shared versions of the Mercury libraries, comment
# out the next two lines and uncomment the two following them.
#
# Note that we specify the example mercury_lib library to the linker separately
# here.  This is because we have not installed it.
#
MERCURY_LINKAGE = --mercury-linkage static
MERCURY_LIB_LDFLAGS = libmercury_lib.a

# For using shared libraries.  (Remember to comment out the versions
# above if you use these.)
# 
#MERCURY_LINKAGE = --mercury-linkage shared
#MERCURY_LIB_LDFLAGS = -L. -Wl,-rpath . -lmercury_lib

# Ask the Mercury compiler what C compiler we should use?
#
CC = $(shell $(MMC) --output-cc)

# Ask the Mercury compiler what flags it passes to the C compiler?
#
CFLAGS=$(shell $(MMC) $(GRADEOPT) --output-cflags)

# Ask the Mercury compiler what command it uses to invoke the linker when
# creating an executable?
#
LD = $(shell $(MMC) --output-link-command)

# Ask the Mercury compiler what flags it passes to the linker in order to link against
# the selected set of Mercury libraries?
#
LIB_LDFLAGS = $(shell $(MMC) $(GRADEOPT) $(MERCURY_LINKAGE) --output-library-link-flags)

# Build the example Mercury library, mercury_lib.
# The dependency on the .init file is merely a convenience.  It's a good
# one to choose since it will always be regenerated if one of the .m files
# in a library is changed.
#
# In order to keep the command lines in this makefile sane we don't
# bother installing it.  Usually we would be working with an installed
# library.
#
mercury_lib.init: mercury_lib.m
	$(MMC) $(GRADEOPT) --make libmercury_lib

# The following rule creates the stand-alone interface to the mercury_lib
# library, Mercury standard library and Mercury runtime.  Since we haven't
# installed mercury_lib all the relevant files will have been built in
# this directory; with an installed library we would need to use the
# `--mld' option to specify its location.
#
mercury_lib_int.o: mercury_lib.init
	$(MMC) $(GRADEOPT) --ml mercury_lib \
		--generate-standalone-interface mercury_lib_int

c_main.o: c_main.c mercury_lib.init mercury_lib_int.o
	$(CC) $(CFLAGS) -c c_main.c

c_main: c_main.o mercury_lib_int.o mercury_lib.init
	$(LD) -o c_main c_main.o $(MERCURY_LIB_LDFLAGS) mercury_lib_int.o $(LIB_LDFLAGS)

.PHONY: realclean
realclean:
	-$(MMC) --make mercury_lib.realclean
	/bin/rm -f mercury_lib_int.[cho] c_main.o c_main Deep.data
	/bin/rm -rf Mercury
