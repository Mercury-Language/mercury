#-----------------------------------------------------------------------------#

main_target: check

#-----------------------------------------------------------------------------#

include ../../compiler/Makefile.mercury
include ../Makefile.tests

MC=mmc
# ML=MERCURY_CLIB_DIR=../../code ml

#-----------------------------------------------------------------------------#

PROGS=	cqueens crypt deriv deriv2 nrev poly primes qsort queens \
	tak query
OUTS=	$(PROGS:%=%.out)
EXPS=	$(PROGS:%=%.exp)
RESS=	$(PROGS:%=%.res)
MODS=	$(PROGS:%=%.mod)

check: $(EXPS) $(OUTS) $(RESS)

mods: $(MODS)

$(MODS): printlist.int

all:	$(PROGS)

#-----------------------------------------------------------------------------#

realclean: clean clean_progs

clean_progs:
	rm -f $(PROGS)

#-----------------------------------------------------------------------------#
