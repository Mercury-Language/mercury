##########
#
#	$Id: Makefile,v 1.2 1997-01-29 06:17:05 aet Exp $
#
#	Makefile for Mercury bytecode interpreter.
#
#	Copyright: The University of Melbourne, 1996
##########

SHELL	= /bin/sh

HDR	= mbi.h util.h disasm.h bytecode.h

SRC1 	= mbi.c 
OBJ1	= mbi.o 
PROG1	= mbi

SRC2	= mdis.c disasm.c
OBJ2	= mdis.o disasm.o
PROG2	= mdis

SRC_COMMON = util.c bytecode.c
OBJ_COMMON = util.o bytecode.o

RM	= /bin/rm -f

CC	= cc 
CFLAGS	= 
DEBUG_FLAGS = -g

CC	= gcc 
CFLAGS	= -Wall -ansi -Wmissing-prototypes -D_POSIX_SOURCE -D__STDC__
DEBUG_FLAGS = -ggdb

MAKEDEPEND=makedepend

LIBPATH	= -L.
INCPATH	= -I.

LIBS1	=
LIBS2	=

##########

.c.o:	
	$(CC) $(DEBUG_FLAGS) $(CFLAGS) $(INCPATH) -c $<

all:	$(PROG1) $(PROG2)

$(PROG1):	$(OBJ1) $(OBJ_COMMON)
	$(CC) $(DEBUG_FLAGS) $(CFLAGS) -o $@ $(OBJ1) $(OBJ_COMMON) \
		$(LIBPATH) $(LIBS1)

$(PROG2):	$(OBJ2) $(OBJ_COMMON)
	$(CC) $(DEBUG_FLAGS) $(CFLAGS) -o $@ $(OBJ2) $(OBJ_COMMON) \
		$(LIBPATH) $(LIBS2)

clean:
	-$(RM) $(OBJ1) $(OBJ2) $(OBJ_COMMON)

realclean: clean
	-$(RM) $(PROG1) $(PROG2)

# Create dependencies. 
depend:	$(SRC1) $(SRC2) $(SRC_COMMON) $(HDR) 
	if test ! -f depend.mk ; then touch depend.mk ; else true ; fi
	$(MAKEDEPEND) -f depend.mk $(SRC1) $(SRC2) $(SRC_COMMON) $(HDR)

-include depend.mk

##########
