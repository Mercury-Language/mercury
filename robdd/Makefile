#-----------------------------------------------------------------------------#
# Copyright (C) 2001-2004,2007 University of Melbourne. 
# This file may only be copied under the terms of the GNU General
# Public Licence - see the file COPYING in the Mercury distribution.
#-----------------------------------------------------------------------------#
#  File     : Makefile
#  Author   : Peter Schachte
#  Origin   : 1995
#  Purpose  : Makefile for bryant graph (ROBDD) manipulation code


TESTS=	test_abexit test_abunify test_iff test_glb test_rglb test_rename \
	test_restrict test_var test_vars
TESTOS=	test_abexit.o test_abunify.o test_iff.o test_glb.o test_rglb.o \
	test_rename.o test_restrict.o test_var.o test_vars.o

COMMONOS=bryant.o bryantPrint.o
SOOS=$(COMMONOS)
OS=$(COMMONOS) timing.o
RELFILES=bryant.c bryant.h bryantPrint.c var.h Makefile \
	 test_abexit.c test_iff.c test_rglb.c test_abunify.c test_rename.c \
	 test_var.c test_glb.c test_restrict.c test_vars.c test_upclose.c
GENFILES=$(COMMONOS) $(TESTOS)

# Amiga options
#LINKSW=LINK PNAME
#DEF=DEFINE=
#MSGS=
#RM=delete quiet
#MV=move clone
#MKDIR=makedir
#DEBUG=DEBUG=SF NOOPT DEFINE=DEBUGALL
#OPTIMIZE=OPTIMIZE NODEBUG DEFINE=NDEBUG
#MAKE=smake
#LIBS=
#SOLIBS=

# Unix options
LINKSW=-o
DEF=-D
MSGS=
RM=rm -f
MV=mv
MKDIR=mkdir
DEBUG=-g -DDEBUGALL
#OPTIMIZE=-O3 -DNDEBUG -funroll-loops
OPTIMIZE=-DNDEBUG
CC=gcc -I../mylib -I/usr/ucbinclude -I. -ansi
LIBDIRS=-L/home/staff/pets/quintus/bin3.2/sun4-5
#LIBS= -L/usr/ucblib -lucb
LIBS=
SOLIBS=
OLIBS=-lqp
SOOPTS=-fpic


# General options

CHOSENOPTIM=$(OPTIMIZE)
#CHOSENOPTIM=$(DEBUG)

#OPTS=$(DEF)COMPUTED_TABLE $(DEF)EQUAL_TEST
OPTS= \
	$(DEF)CLEAR_CACHES \
	$(DEF)COMPUTED_TABLE \
	$(DEF)EQUAL_TEST \
	$(DEF)USE_ITE_CONSTANT
#OPTS=$(DEF)CLEAR_CACHES $(DEF)COMPUTED_TABLE $(DEF)EQUAL_TEST $(DEF)STATISTICS
#WHICH=NAIVE
#WHICH=OLD
#WHICH=USE_THRESH
#WHICH=USE_RGLB
WHICH=NEW

QOPT=
#QOPT=$(DEF)QUINTUS

RSET=$(DEF)RESTRICT_SET


# Main makefile

CFLAGS=$(CHOSENOPTIM) $(DEF)$(WHICH) $(OPTS) $(RSET) $(QOPT) $(MSGS) $(EXTRAOPTS)

all: $(TESTS)

everything: alltests allsos

alltests: naive old thresh rglb new

naive-enum:	naive-enumtests
	$(MAKE) clean
	$(MAKE) WHICH=NAIVE RSET= all
	$(RM) naive-enumtests/*
	$(MV) $(TESTS) naive-enumtests

old-enum:	old-enumtests
	$(MAKE) clean
	$(MAKE) WHICH=OLD RSET= all
	$(RM) old-enumtests/*
	$(MV) $(TESTS) old-enumtests

naive:	naivetests
	$(MAKE) clean
	$(MAKE) WHICH=NAIVE all
	$(RM) naivetests/*
	$(MV) $(TESTS) naivetests

old:	oldtests
	$(MAKE) clean
	$(MAKE) WHICH=OLD all
	$(RM) oldtests/*
	$(MV) $(TESTS) oldtests

thresh:	threshtests
	$(MAKE) clean
	$(MAKE) WHICH=USE_THRESH all
	$(RM) threshtests/*
	$(MV) $(TESTS) threshtests

rglb:	rglbtests
	$(MAKE) clean
	$(MAKE) WHICH=USE_RGLB all
	$(RM) rglbtests/*
	$(MV) $(TESTS) rglbtests

new:	newtests
	$(MAKE) clean
	$(MAKE) WHICH=NEW all
	$(RM) newtests/*
	$(MV) $(TESTS) newtests


allsos:	naiveso oldso threshso rglbso newso bryant.so

naiveso:	naivetests
	$(MAKE) clean
	$(MAKE) WHICH=NAIVE QOPT=$(DEF)QUINTUS EXTRAOPTS=$(SOOPTS) rep.so
	$(RM) naivetests/rep.so
	$(MV) rep.so naivetests

oldso:	oldtests
	$(MAKE) clean
	$(MAKE) WHICH=OLD QOPT=$(DEF)QUINTUS EXTRAOPTS=$(SOOPTS) rep.so
	$(RM) oldtests/rep.so
	$(MV) rep.so oldtests

threshso:	threshtests
	$(MAKE) clean
	$(MAKE) WHICH=USE_THRESH QOPT=$(DEF)QUINTUS EXTRAOPTS=$(SOOPTS) rep.so
	$(RM) threshtests/rep.so
	$(MV) rep.so threshtests

rglbso:	rglbtests
	$(MAKE) clean
	$(MAKE) WHICH=USE_RGLB QOPT=$(DEF)QUINTUS EXTRAOPTS=$(SOOPTS) rep.so
	$(RM) rglbtests/rep.so
	$(MV) rep.so rglbtests

newso:	newtests
	$(MAKE) clean
	$(MAKE) WHICH=NEW QOPT=$(DEF)QUINTUS EXTRAOPTS=$(SOOPTS) rep.so
	$(RM) newtests/rep.so
	$(MV) rep.so newtests


bryant.so:
	$(MAKE) clean
	$(MAKE) WHICH=NEW QOPT=$(DEF)QUINTUS EXTRAOPTS=$(SOOPTS) $(SOOS)
	gcc -shared -o $@ $(SOOS) $(SOLIBS)


naive-enumtests old-enumtests naivetests oldtests threshtests rglbtests newtests:
	$(MKDIR) $@


bryantPrint.o: bryantPrint.c bryant.h bryantPrint.h

bryant.o: bryant.c bryant.h

test_abexit.o:	test_abexit.c bryant.h timing.h

test_abunify.o:	test_abunify.c bryant.h timing.h

test_iff.o:	test_iff.c bryant.h timing.h

test_glb.o:	test_glb.c bryant.h timing.h

test_rglb.o:	test_rglb.c bryant.h timing.h

test_rename.o:	test_rename.c bryant.h timing.h

test_restrict.o:	test_restrict.c bryant.h timing.h

test_var.o:	test_var.c bryant.h timing.h

test_vars.o:	test_vars.c bryant.h timing.h


rep.so: $(SOOS)
	gcc -shared -o $@ $(SOOS) $(SOLIBS)

rep.o:	$(SOOS)
	gcc -o $@ $(LIBDIRS) $(SOOS) $(OLIBS)

test:	test.o $(OS)
	$(CC) $(LINKSW) $@ test.o $(OS) $(LIBS) LINK

test_abexit:	test_abexit.o $(OS)
	$(CC) $(LINKSW) $@ test_abexit.o $(OS) $(LIBS)

test_abunify:	test_abunify.o $(OS)
	$(CC) $(LINKSW) $@ test_abunify.o $(OS) $(LIBS)

test_iff:	test_iff.o $(OS)
	$(CC) $(LINKSW) $@ test_iff.o $(OS) $(LIBS)

test_glb:	test_glb.o $(OS)
	$(CC) $(LINKSW) $@ test_glb.o $(OS) $(LIBS)

test_rglb:	test_rglb.o $(OS)
	$(CC) $(LINKSW) $@ test_rglb.o $(OS) $(LIBS)

test_rename:	test_rename.o $(OS)
	$(CC) $(LINKSW) $@ test_rename.o $(OS) $(LIBS)

test_restrict:	test_restrict.o $(OS)
	$(CC) $(LINKSW) $@ test_restrict.o $(OS) $(LIBS)

test_var:	test_var.o $(OS)
	$(CC) $(LINKSW) $@ test_var.o $(OS) $(LIBS)

test_vars:	test_vars.o $(OS)
	$(CC) $(LINKSW) $@ test_vars.o $(OS) $(LIBS)

test_upclose:	test_upclose.o $(OS)
	$(CC) $(LINKSW) $@ test_upclose.o $(OS) $(LIBS)

abex:	$(OS) abex.o
	$(CC) $(LINKSW) $@ $@.o $(OS) $(LIBS)

clean:
	- $(RM) $(GENFILES)

distrib:	bryant.tgz

bryant.tgz:	$(RELFILES)
	-$(RM) $@
	tar cvfz $@ $(RELFILES)

tidy:	clean
	- $(RM) $(TESTS)
	- $(RM) *~
