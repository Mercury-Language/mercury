.PHONY: all

all: sudoku test_eqneq

sudoku: sudoku.m eqneq.m
	mmc --make sudoku --use-grade-subdirs --use-trail

test_eqneq: test_eqneq.m eqneq.m
	mmc --make test_eqneq --use-grade-subdirs --use-trail
