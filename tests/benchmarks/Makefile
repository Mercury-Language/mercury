#-----------------------------------------------------------------------------#

main_target: all

#-----------------------------------------------------------------------------#

MLIB=/usr/contrib/lib/mercury
GRADE=fast
MCFLAGS=--optimize --gcc-global-registers --gcc-non-local-gotos

VPATH=$(MLIB)/nuprolog/$(shell fullarch):$(MLIB)/ints

#-----------------------------------------------------------------------------#

TESTS=cqueens crypt deriv deriv2 nrev poly primes printlist qsort queens tak
	# `query' doesn't work

all : $(TESTS)

include Makefile.mercury

#-----------------------------------------------------------------------------#

.PRECIOUS: %.mod
.PRECIOUS: %.c
.PRECIOUS: %.o
.PRECIOUS: %_init.c

%.mod: printlist.int

%_init.c: %.mod printlist.mod

%: %.o %_init.o printlist.o
	MERCURY_CLIB_DIR=$(HOME)/thesis/imp/code \
	ml -s$(GRADE) -o $@ $^

%.out: %
	./$< > $@ 2>&1

%.nu : %.no printlist.no
	mnl -o $@ $^

clean:
	rm -f *.o *.no *.mod *.d *_init.c *.c

realclean: clean
	rm -f $(TESTS)

-include *.d

#-----------------------------------------------------------------------------#
