#-----------------------------------------------------------------------------#

main_target: check

#-----------------------------------------------------------------------------#

MLIB=/usr/contrib/lib/mercury
#GRADE=fast
#MCFLAGS=--optimize --gcc-global-registers --gcc-non-local-gotos
GRADE=debug
MCFLAGS=--debug

VPATH=$(MLIB)/nuprolog/$(shell fullarch):$(MLIB)/ints

#-----------------------------------------------------------------------------#

PROGS=	cqueens crypt deriv deriv2 nrev poly primes qsort queens \
	tak query
OUTS=	$(PROGS:%=%.out)
EXPS=	$(PROGS:%=%.exp)
RESS=	$(PROGS:%=%.res)

check: $(EXPS) $(OUTS) $(RESS)

all:	$(PROGS)

include Makefile.mercury
MERCC=mmc

#-----------------------------------------------------------------------------#

.PRECIOUS: %.mod %.c %.o %_init.c %.no %.nu %_init.nl %_init.no

%.mod: printlist.int

%_init.c: %.mod printlist.mod engine.mod wrapper.mod io.mod Entry

%_init.nl: NP_Entry
	echo "main(_) :- `extract $* '' NP_Entry`." > $@

%: %.o %_init.o printlist.o
	MERCURY_CLIB_DIR=$(HOME)/thesis/imp/code \
	ml -s$(GRADE) -o $@ -L . $^

%: %.o
%: %.c
%: %.mod

%.out: %
	./$< > $@ 2>&1

%.exp: %.nu
	./$< > $@ 2>&1

%.res: %.exp %.out
	diff -u $*.exp $*.out > $@

%.nu : %.no %_init.no printlist.no
	mnl -o $@ $^

clean:
	rm -f *.o *.no *.nu *.nu.save *.mod *.d *_init.c *.c *.out *.exp \
	*_init.c *_init.nl *.res

realclean: clean clean_progs

clean_progs:
	rm -f $(PROGS)

-include *.d

#-----------------------------------------------------------------------------#
