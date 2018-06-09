/*
** Copyright (C) 1993, 1997, 1999 The University of Melbourne.
** Copyright (C) 2018 The Mercury team.
** This file is distributed under the terms specified in COPYING.LIB.
*/

/*
** regtest2.c - part of the regtest.c program.
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

extern FILE *f;
int global;

void extern_clobber_regs_in_func(void);
void extern_call_func_with_args(
	int x1, int x2, int x3, int x4,
	int y1, int y2, int y3, int y4,
	double f1, double f2, double f3, double f4
);

void 
extern_call_func_with_args(
	int x1, int x2, int x3, int x4,
	int y1, int y2, int y3, int y4,
	double f1, double f2, double f3, double f4
) {
	assert(
		x1 == 1 &&
		x2 == 2 &&
		x3 == 3 &&
		x4 == 4 &&
		y1 == 5 &&
		y2 == 6 &&
		y3 == 7 &&
		y4 == 8 &&
		f1 == 10.0 &&
		f2 == 20.0 &&
		f3 == 30.0 &&
		f4 == 40.0
	);
}

/* Do a variety of stuff that might clobber the registers */

#define clobber_regs()				\
do {						\
	double x = 1.5;				\
	int i = 35;				\
						\
	fprintf(f, "Hello, world %d\n", i);	\
	x = sin(x + global);			\
	global = (int) x;			\
	x = pow(x,1.5);				\
	malloc(100);				\
	extern_call_func_with_args(1,2,3,4,5,6,7,8,10.0,20.0,30.0,40.0); \
	system("/bin/true");			\
} while (0)

void 
extern_clobber_regs_in_func(void) {
	clobber_regs();
}

