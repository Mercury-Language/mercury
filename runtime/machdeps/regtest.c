/*
** Copyright (C) 1993, 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
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

FILE *f;

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

	return 1;
}

int 
main(void) {

	f = fopen("/dev/null", "w");
	if (!f) {
		printf("can't open /dev/null?\n");
		return 0;
	}

	if (test_reg()) {
		printf("Register %s seems to work ok.\n", REG);
	} else {
		printf("Register %s got clobbered.\n", REG);
	}

   return 0;
}
