/*
** Copyright (C) 1995,2003-2004 Peter Schachte and The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*****************************************************************
  File     : test_abunify.c
  Author   : Peter Schachte
  Origin   : Fri Aug  4 14:39:44 1995
  Purpose  : Timing test for bryant graph MR_ROBDD_abstract_unify code

*****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "bryant.h"
#include "timing.h"


int opcount;

void usage(char *progname)
    {
	printf("usage:  %s size maxvar [repetitions]\n", progname);
	printf("  for all possible v <-> v1 & v2 & ... & vsize functions, computes the MR_ROBDD_glb\n");
	printf("  of that and each possible v <-> v1 & v2 & ... & vsize function, restricted\n");
	printf("  to each threshold between 0 and maxvar.  Each v and the vi are between 0\n");
	printf("  and maxvar inclusive.  If repetitions is >0, this will be done that many\n");
	printf("  times.\n");
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


void doit(int thresh, int varmax, MR_ROBDD_type *f, int v0, int n, int arr[])
    {
	MR_ROBDD_type *result;
#ifdef DEBUGALL
	int i;

	printf("MR_ROBDD_abstract_unify(");
	printOut(f);
	printf(", %d, %d, [", v0, n);
	if (n>0) {
	    printf("%d", arr[0]);
	    for (i=1; i<n; ++i) printf(",%d", arr[i]);
	}
	printf("], %d) =", thresh);
	fflush(stdout);
#endif /* DEBUGALL */
#ifndef OVERHEAD
#if !defined(MR_ROBDD_USE_THRESH) && !defined(MR_ROBDD_RESTRICT_SET)
	result = MR_ROBDD_abstract_unify(f, v0, n, arr, thresh, varmax);
#else /* MR_ROBDD_USE_THRESH */
	result = MR_ROBDD_abstract_unify(f, v0, n, arr, thresh);
#endif /* MR_ROBDD_OLD */
#ifdef DEBUGALL
	printOut(result);
	printf("\n");
#endif /* DEBUGALL */
#endif /* !OVERHEAD */
	++opcount;
    }


void dont_doit(int thresh, int varmax, MR_ROBDD_type *f, int v0, int n, int arr[])
    {
    }


void inner_loop(int varmax, int top, int thresh, MR_ROBDD_type *f)
    {
	int arrayg[MR_ROBDD_MAXVAR];
	int vg;

	for (vg=0; vg<varmax; ++vg) {
	    init_array(top, vg, arrayg);
	    doit(thresh, varmax, f, vg, top, arrayg);
	    while (next_array(top, varmax, vg, arrayg)) {
		doit(thresh, varmax, f, vg, top, arrayg);
	    }
	}
    }



void dont_inner_loop(int varmax, int top, int thresh, MR_ROBDD_type *f)
    {
	int arrayg[MR_ROBDD_MAXVAR];
	int vg;

	for (vg=0; vg<varmax; ++vg) {
	    init_array(top, vg, arrayg);
	    dont_doit(thresh, varmax, f, vg, top, arrayg);
	    while (next_array(top, varmax, vg, arrayg)) {
		dont_doit(thresh, varmax, f, vg, top, arrayg);
	    }
	}
    }



int main(int argc, char **argv)
    {
	int varmax, size, repetitions;
	int arrayf[MR_ROBDD_MAXVAR];
	int reps, vf, thresh;
	MR_ROBDD_type *f;
	millisec clock0, clock1, clock2, clock3;
	float runtime, overhead, rate;
	int test_nodes, overhead_nodes;

	if (argc < 3) {
	    usage(argv[0]);
	    return 20;
	}
	if ((varmax=atoi(argv[2]))<1 || varmax>=MR_ROBDD_MAXVAR) {
	    usage(argv[0]);
	    printf("\n  varmax must be between 1 <= varmax < %d\n", MR_ROBDD_MAXVAR);
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
	    for (thresh=0; thresh<=varmax; ++thresh) {
		for (vf=0; vf<varmax; ++vf) {
		    init_array(size, vf, arrayf);
		    f = MR_ROBDD_testing_iff_conj_array(vf, size, arrayf);
		    inner_loop(varmax, size, thresh, f);
		    while (next_array(size, varmax, vf, arrayf)) {
			f = MR_ROBDD_testing_iff_conj_array(vf, size, arrayf);
			inner_loop(varmax, size, thresh, f);
		    }
		}
	    }
	}
	clock1 = milli_time();
	test_nodes = MR_ROBDD_nodes_in_use();
	MR_ROBDD_initRep();
	clock2 = milli_time();
	for (reps=repetitions; reps>0; --reps) {
	    for (thresh=0; thresh<=varmax; ++thresh) {
		for (vf=0; vf<varmax; ++vf) {
		    init_array(size, vf, arrayf);
		    f = MR_ROBDD_testing_iff_conj_array(vf, size, arrayf);
		    dont_inner_loop(varmax, size, thresh, f);
		    while (next_array(size, varmax, vf, arrayf)) {
			f = MR_ROBDD_testing_iff_conj_array(vf, size, arrayf);
			dont_inner_loop(varmax, size, thresh, f);
		    }
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
