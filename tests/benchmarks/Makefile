#-----------------------------------------------------------------------------#

main_target: check

#-----------------------------------------------------------------------------#

include ../../compiler/Makefile.mercury
include ../Makefile.tests

MC=mmc

#-----------------------------------------------------------------------------#

PROGS=	cqueens crypt deriv deriv2 nrev poly primes qsort queens \
	tak query
OUTS=	$(PROGS:%=%.out)
EXPS=	$(PROGS:%=%.exp)
RESS=	$(PROGS:%=%.res)

check: $(EXPS) $(OUTS) $(RESS)

all:	$(PROGS)

#-----------------------------------------------------------------------------#

realclean: clean clean_progs

clean_progs:
	rm -f $(PROGS)

#-----------------------------------------------------------------------------#

%_init.c: %.mod
	$(MOD2INIT) $(MOD2INITFLAGS)  /usr/contrib/lib/mercury/modules/* printlist.mod $< > $@
