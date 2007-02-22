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
#
# NOTE: the following example is statically linked against the Mercury
# libraries.  This is in order to make the example as portable as possible.
# Shared libraries will also work with stand-alone interfaces on systems
# that support them.

all: c_main

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
	mmc --make libmercury_lib

# The following rule creates the stand-alone interface to the mercury_lib
# library, Mercury standard library and Mercury runtime.  Since we haven't
# installed mercury_lib all the relevant files will have been built in
# this directory; with an installed library we would need to use the
# `--mld' option to specify its location.
#
mercury_lib_int.o: mercury_lib.init
	mmc --mercury-linkage static --ml mercury_lib \
		--generate-standalone-interface mercury_lib_int

# We use the mgnuc script here rather than invoking gcc directly since
# c_main.c #includes parts of the Mercury runtime and mgnuc knows where to
# find those header files without us specifying the location.  If we used
# gcc (or some other C compiler) directly then we would need to specify the
# location of the runtime and standard library header files, e.g. by using
# the `-I' option.
#
c_main.o: c_main.c mercury_lib.init mercury_lib_int.o
	mgnuc -c c_main.c

# For similar reasons we use the ml script rather than invoking ld directly.
#
c_main: c_main.o mercury_lib_int.o mercury_lib.init
	ml --mercury-libs static -o c_main \
		c_main.o libmercury_lib.a mercury_lib_int.o	

.PHONY: realclean
realclean:
	-mmc --make mercury_lib.realclean
	/bin/rm -f mercury_lib_int.[cho] c_main.o c_main
	/bin/rm -rf Mercury
