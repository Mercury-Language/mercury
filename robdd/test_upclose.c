/*
** Copyright (C) 1998,2003-2004 Peter Schachte and The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*****************************************************************
    File   : test_upclose
    Author : Peter Schachte
    Origin : 4 May 1998
    Purpose: Test of set sharing upward closure operation

*****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "bryant.h"
#include "timing.h"


int opcount;

void usage(char *progname)
    {
	printf("usage:  %s depth [repetitions]\n", progname);
	printf("  creates random boolean functions of depth depth (or less) and\n");
	printf("  computes their upward closure.  If repetitions is >0,\n");
	printf("  this will be done that many times.\n");
    }


#if 0
MR_ROBDD_node *boolfn(unsigned long n)
    {
	int d;
	unsigned long tr, fa;
	MR_ROBDD_node *trfn, *fafn;

	if (n == 0) return MR_ROBDD_zero;
	if (n == 1) return MR_ROBDD_one;

	for (d = MR_ROBDD_LOG_BITS_PER_WORD-1; (fa=(n>>(1<<d)))==0 && d >= 0; --d);
	tr = n & ((1<<(1<<d))-1);
	trfn = boolfn(tr);
	fa ^= tr;
	fafn = boolfn(fa);
	return MR_ROBDD_make_node((MR_ROBDD_LOG_BITS_PER_WORD-d), trfn, fafn);
    }
#endif

int rnd;
int bits;
int rand_bits;

void init_random(void)
    {
        int i;

	for (i=RAND_MAX, rand_bits=0; i!=0; i>>=1) ++rand_bits;
	bits = 0;
	srand(4242);
    }


MR_ROBDD_node *randboolfn(int depth, int level)
    {
	MR_ROBDD_node *result;

	if (bits <= 0) rnd = rand(), bits = rand_bits;

	if (level == 0) {
	    result = (rnd&1) ? MR_ROBDD_one : MR_ROBDD_zero;
	    bits--; rnd >>= 1;
	} else {
	    result = MR_ROBDD_make_node(depth-level,
			       randboolfn(depth, level-1),
			       randboolfn(depth, level-1));
	}
	return result;
    }

void doit(int depth, int num)
    {
	MR_ROBDD_node *f = randboolfn(depth, depth);
	MR_ROBDD_node *uf;

#ifdef DEBUGALL
	printf("%6d MR_ROBDD_upclose(", num);
	printOut(f);
	printf(") ==> ");
	fflush(stdout);
#endif /* DEBUGALL */
#ifndef OVERHEAD
	uf = MR_ROBDD_upclose(f);
#ifdef DEBUGALL
	if (f == uf) {
	    printf("UNCHANGED");
	} else {
	    printOut(uf);
	}
	printf("\n");
#endif /* DEBUGALL */
#endif /* !OVERHEAD */
	++opcount;
    }


void dont_doit(int depth, int num)
    {
	(void) randboolfn(depth, depth);
    }


int main(int argc, char **argv)
    {
	int depth, repetitions;
	int reps;
	millisec clock0, clock1, clock2, clock3;
	float runtime, overhead, rate;
	int test_nodes, overhead_nodes;

	if (argc < 2) {
	    usage(argv[0]);
	    return 20;
	}
	if ((depth=atoi(argv[1]))<1 || depth>=MR_ROBDD_MAXVAR) {
	    usage(argv[0]);
	    printf("\n  depth must be between 1 <= depth < %d\n", MR_ROBDD_MAXVAR);
	    return 20;
	}
	repetitions=(argc>2 ? atoi(argv[2]) : 1);
	if (repetitions <= 0) repetitions = 1;

	opcount = 0;
	MR_ROBDD_initRep();
	init_random();
	clock0 = milli_time();
	for (reps=0; reps<repetitions; ++reps) {
	    doit(depth, reps);
	}
	clock1 = milli_time();
	test_nodes = MR_ROBDD_nodes_in_use();
	MR_ROBDD_concludeRep();
	MR_ROBDD_initRep();
	init_random();
	clock2 = milli_time();
	for (reps=0; reps<repetitions; ++reps) {
	    dont_doit(depth, reps);
	}
	clock3 = milli_time();
	overhead_nodes = MR_ROBDD_nodes_in_use();
	runtime = (float)(clock1-clock0)/1000;
	overhead = (float)(clock3-clock2)/1000;
	rate = ((float)opcount)/(runtime-overhead);
	printf("%s %d %d:  %.3f - %.3f = %.3f secs, %d ops, %d nodes, %.1f ops/sec\n",
	       argv[0], depth, repetitions,
	       runtime, overhead, (runtime-overhead), opcount,
	       test_nodes-overhead_nodes, rate);
	return 0;
    }
