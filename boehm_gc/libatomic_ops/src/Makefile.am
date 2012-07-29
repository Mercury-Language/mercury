SUBDIRS=atomic_ops

AM_CFLAGS=@PICFLAG@
AM_CPPFLAGS = -I$(top_builddir)/src -I$(top_srcdir)/src

# Disable the build of libatomic_ops_gpl.a as the Mercury compiler
# doesn't need it and it doesn't build under Sparc Solaris 10
#include_HEADERS=atomic_ops.h atomic_ops_stack.h atomic_ops_malloc.h
#lib_LIBRARIES = libatomic_ops.a libatomic_ops_gpl.a
include_HEADERS=atomic_ops.h
lib_LIBRARIES = libatomic_ops.a
if NEED_ASM
libatomic_ops_a_SOURCES = atomic_ops.c atomic_ops_sysdeps.S
else
libatomic_ops_a_SOURCES = atomic_ops.c
endif

# Disable the build of libatomic_ops_gpl.a as the Mercury compiler
# doesn't need it and it doesn't build under Sparc Solaris 10
#libatomic_ops_gpl_a_SOURCES = atomic_ops_stack.c atomic_ops_malloc.c

EXTRA_DIST=Makefile.msft

