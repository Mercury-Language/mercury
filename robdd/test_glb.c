/*
** Copyright (C) 1995,2003-2004 Peter Schachte and The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*****************************************************************
    File   : test_glb.c
    Author : Peter Schachte
    Origin : Thu Jul 13 14:25:12 1995
    Purpose: Timing test for bryant graph MR_ROBDD_glb code

*****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "bryant.h"
#include "timing.h"

#define VARLIMIT 1024

int opcount;

void usage(char *progname)
    {
	printf("usage:  %s size maxvar [repetitions]\n", progname);
	printf("  creates all possible pairs of v <-> v1 & v2 & ... & vsize functions and\n");
	printf("  computes their MR_ROBDD_glb.  Each V and the vi are between 0 and maxvar inclusive.\n");
	printf("  If repetitions is >0, this will be done that many times.\n");
    }


void init_array(int top, int v0, int array[])
    {
	int i, val;

	for (i=0, val=0; i<top; ++i, ++val) {
	    if (val==v0) ++val;
	    array[i] = val;
	}
    }



int next_array(int n, int varmax, int v0, int array[])
    {
	int i;
	int limit;
	int val;

	/* Search backward for first cell with "room" to be
	 * incremented.  This is complicated by the need to avoid
	 * using the value v0.
	 */
	for (i=n-1, limit=varmax-1;; --i, --limit) {
	    if (i<0) return 0;	/* no more combinations possible */
	    if (limit==v0) --limit;
	    if (++array[i]==v0) ++array[i];
	    if (array[i]<=limit) break;
	}
	/* Now we've incremented array[i], and must set
	 * array[i+1..n-1] to successive values (avoiding v0).
	 */
	for (val=array[i]+1, ++i; i<n; ++i, ++val) {
	    if (val==v0) ++val;
	    array[i] = val;
	}
	return 1;
    }


void doit(MR_ROBDD_type *f, MR_ROBDD_type *g)
    {
	MR_ROBDD_type *result;
#ifdef DEBUGALL
	printf("MR_ROBDD_glb("),
	printOut(f),
	printf(", ");
	printOut(g),
	printf(") = ");
	fflush(stdout);
#endif /* DEBUGALL */
#ifndef OVERHEAD
	result = MR_ROBDD_glb(f, g);
#endif /* !OVERHEAD */
#ifdef DEBUGALL
	printOut(result);
	printf("\n");
#endif /* DEBUGALL */
	++opcount;
    }

void dont_doit(MR_ROBDD_type *f, MR_ROBDD_type *g)
    {
    }

void inner_loop(int varmax, int top, MR_ROBDD_type *f)
    {
	int arrayg[33];
	int vg;
	MR_ROBDD_type *g;

	for (vg=0; vg<varmax; ++vg) {
	    init_array(top, vg, arrayg);
	    g = MR_ROBDD_testing_iff_conj_array(vg, top, arrayg);
	    doit(f, g);
	    while (next_array(top, varmax, vg, arrayg)) {
		g = MR_ROBDD_testing_iff_conj_array(vg, top, arrayg);
		doit(f, g);
	    }
	}
    }



void dont_inner_loop(int varmax, int top, MR_ROBDD_type *f)
    {
	int arrayg[33];
	int vg;
	MR_ROBDD_type *g;

	for (vg=0; vg<varmax; ++vg) {
	    init_array(top, vg, arrayg);
	    g = MR_ROBDD_testing_iff_conj_array(vg, top, arrayg);
	    dont_doit(f, g);
	    while (next_array(top, varmax, vg, arrayg)) {
		g = MR_ROBDD_testing_iff_conj_array(vg, top, arrayg);
		dont_doit(f, g);
	    }
	}
    }



int main(int argc, char **argv)
    {
	int varmax, size, repetitions;
	int arrayf[VARLIMIT];
	int reps, vf;
	MR_ROBDD_type *f;
	millisec clock0, clock1, clock2, clock3;
	float runtime, overhead, rate;
	int test_nodes, overhead_nodes;

	if (argc < 3) {
	    usage(argv[0]);
	    return 20;
	}
	if ((varmax=atoi(argv[2]))<1 || varmax>=VARLIMIT) {
	    usage(argv[0]);
	    printf("\n  varmax must be between 1 <= varmax < %d\n", VARLIMIT);
	    return 20;
	}
	if ((size=atoi(argv[1]))<0 || size>=varmax) {
	    usage(argv[0]);
	    printf("\n  size must be between 0 <= size < varmax\n");
	    return 20;
	}
	repetitions=(argc>3 ? atoi(argv[3]) : 1);
	if (repetitions <= 0) repetitions = 1;

	opcount = 0;
	clock0 = milli_time();
	for (reps=repetitions; reps>0; --reps) {
	    for (vf=0; vf<varmax; ++vf) {
		init_array(size, vf, arrayf);
		f = MR_ROBDD_testing_iff_conj_array(vf, size, arrayf);
		inner_loop(varmax, size, f);
		while (next_array(size, varmax, vf, arrayf)) {
		    f = MR_ROBDD_testing_iff_conj_array(vf, size, arrayf);
		    inner_loop(varmax, size, f);
		}
	    }
	}
	clock1 = milli_time();
	test_nodes = MR_ROBDD_nodes_in_use();
	MR_ROBDD_initRep();
	clock2 = milli_time();
	for (reps=repetitions; reps>0; --reps) {
	    for (vf=0; vf<varmax; ++vf) {
		init_array(size, vf, arrayf);
		f = MR_ROBDD_testing_iff_conj_array(vf, size, arrayf);
		dont_inner_loop(varmax, size, f);
		while (next_array(size, varmax, vf, arrayf)) {
		    f = MR_ROBDD_testing_iff_conj_array(vf, size, arrayf);
		    dont_inner_loop(varmax, size, f);
		}
	    }
	}
	clock3 = milli_time();
	overhead_nodes = MR_ROBDD_nodes_in_use();
	runtime = (float)(clock1-clock0)/1000;
	overhead = (float)(clock3-clock2)/1000;
	rate = ((float)opcount)/(runtime-overhead);
	printf("%s %d %d %d:  %.3f - %.3f = %.3f secs, %d ops, %d nodes, %.1f ops/sec\n",
	       argv[0], size, varmax, repetitions,
	       runtime, overhead, (runtime-overhead), opcount,
	       test_nodes-overhead_nodes, rate);
	return 0;
    }
