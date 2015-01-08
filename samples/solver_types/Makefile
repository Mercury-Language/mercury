#-----------------------------------------------------------------------------#
# vim: ts=8 sw=8 noexpandtab
#-----------------------------------------------------------------------------#

.PHONY: all

all: sudoku test_eqneq

sudoku: sudoku.m eqneq.m
	mmc --make sudoku

test_eqneq: test_eqneq.m eqneq.m
	mmc --make test_eqneq

.PHONY: realclean
realclean:
	mmc --make sudoku.realclean
	mmc --make test_eqneq.realclean
	/bin/rm -rf Mercury
