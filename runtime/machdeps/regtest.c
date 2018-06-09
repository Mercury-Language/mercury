/*
** Copyright (C) 1993, 1997, 1999 The University of Melbourne.
** Copyright (C) 2018 The Mercury team.
** This file is distributed under the terms specified in COPYING.LIB.
*/

/*
** regtest.c - attempt to determine whether a register gets clobbered when you
** call the C standard library
*/

#ifndef REG	/* the register to test */
#error "regtest.c must be compiled with -DREG=\"<register name>\""
#endif

register unsigned r __asm__(REG);

/*
** Any #includes must come *after* the global register variable declaration,
** because some systems define inline functions in header files, and gcc
** requires that global register variable declarations precede all
** function definitions.
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

FILE *f;

/* defined in regtest2.c */
extern void extern_clobber_regs_in_func(void);
extern void extern_call_func_with_args(
	int x1, int x2, int x3, int x4,
	int y1, int y2, int y3, int y4,
	double f1, double f2, double f3, double f4
);

static void 
call_func_with_args(
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
	x = sin(x);				\
	x = pow(x,1.5);				\
	malloc(100);				\
	call_func_with_args(1,2,3,4,5,6,7,8,10.0,20.0,30.0,40.0); \
	extern_call_func_with_args(1,2,3,4,5,6,7,8,10.0,20.0,30.0,40.0); \
	system("/bin/true");			\
} while (0)

static void 
clobber_regs_in_func(void) {
	clobber_regs();
}

/* Test the register to see whether it gets clobbered
   Return 0 if it gets clobbered, 1 if it survives */
static int 
test_reg(void) {

	r = 0x12345678u;
	clobber_regs();
	if (r != 0x12345678u) {
		return 0;
	}

	r = 0x87654321u;
	clobber_regs();
	if (r != 0x87654321u) {
		return 0;
	}

	r = 0x12345678u;
	clobber_regs_in_func();
	if (r != 0x12345678u) {
		return 0;
	}

	r = 0x87654321u;
	clobber_regs_in_func();
	if (r != 0x87654321u) {
		return 0;
	}

	r = 0x12345678u;
	extern_clobber_regs_in_func();
	if (r != 0x12345678u) {
		return 0;
	}

	r = 0x87654321u;
	extern_clobber_regs_in_func();
	if (r != 0x87654321u) {
		return 0;
	}

	return 1;
}

int 
main(void) {

	f = fopen("/dev/null", "w");
	if (!f) {
		printf("can't open /dev/null?\n");
		return 1;
	}

	if (test_reg()) {
		printf("Test of register %s passed.\n", REG);
		return 0;
	} else {
		printf("Register %s got clobbered.\n", REG);
		return 1;
	}
}
