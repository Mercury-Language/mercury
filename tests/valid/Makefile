#-----------------------------------------------------------------------------#

main_target: all

#-----------------------------------------------------------------------------#

include ../../compiler/Makefile.mercury
include ../Makefile.tests

MC=mmc

#-----------------------------------------------------------------------------#

SOURCES=			\
	assoc_list.nl		\
	det_inference.nl	\
	det_switch.nl		\
	easy_nondet_test.nl	\
	easy_nondet_test_2.nl	\
	mode_merge_insts.nl	\
	name_mangling.nl	\
	same_length_2.nl	\
	same_length_3.nl	\
	indexing.nl		\
	tiny.nl

OBJS=	$(SOURCES:%.nl=%.o)
NOS =	$(SOURCES:%.nl=%.no)
PROGS =	$(SOURCES:%.nl=%)

all:	$(OBJS) $(NOS)

tiny: tiny.o tiny_init.o
	$(ML) -o tiny tiny.o tiny_init.o

realclean: clean_progs

clean_progs:
	rm -f $(PROGS)

#-----------------------------------------------------------------------------#
