#-----------------------------------------------------------------------------#

main_target: check

#-----------------------------------------------------------------------------#

include ../../compiler/Makefile.mercury
include ../Makefile.tests

MC=mmc
ML=MERCURY_CLIB_DIR=../../code ml

#-----------------------------------------------------------------------------#

PROGS=	arithmetic string_test # interpreter
OUTS=	$(PROGS:%=%.out)
EXPS=	$(PROGS:%=%.exp)
RESS=	$(PROGS:%=%.res)
MODS=	$(PROGS:%=%.mod)

check: $(EXPS) $(OUTS) $(RESS)

mods: $(MODS)

all:	$(PROGS)

$(MODS) : printlist.int

string_test: string_test.o string_test_init.o string.o char.o list.o
	$(ML) -s$(GRADE) $(MLFLAGS) -o $@ $^
string_test_init.c: string_test.mod string.mod char.mod list.mod \
		/usr/contrib/lib/mercury/modules/*
	$(MOD2INIT) -w"`extract $* '' Entry`" $^ > $@

interpreter: interpreter.o interpreter_init.o varset.o io.o
	$(ML) -s$(GRADE) $(MLFLAGS) -o $@ $^
interpreter_init.c: interpreter.mod varset.mod io.mod \
		/usr/contrib/lib/mercury/modules/*
	$(MOD2INIT) -w"`extract $* '' Entry`" $^ > $@

#-----------------------------------------------------------------------------#

realclean: clean clean_progs

clean_progs:
	rm -f $(PROGS)

#-----------------------------------------------------------------------------#
