/*****************************************************************
  File     : test_rename.c
  RCS      : $Id: test_rename.c,v 1.1.2.2 2000-09-27 04:00:11 dgj Exp $
  Author   : Peter Schachte
  Origin   : Sat Jul 29 20:53:11 1995
  Purpose  : Timing test for bryant graph renameArray code

*****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "bryant.h"
#include "timing.h"


int opcount;

void usage(char *progname)
    {
	printf("usage:  %s size maxvar [repetitions]\n", progname);
	printf("  does all possible renamings of a certain boolean function of the specified\n");
	printf("  size using variables 0..maxvar inclusive.  If repetitions is >0, this will\n");
	printf("  be done that many times.\n");
    }


void init_array(int top, int array[], bitset *usedvars)
    {
	int i, word;
	bitmask mask;

	BITSET_CLEAR(*usedvars);
	FOREACH_POSSIBLE_ELEMENT(i, word, mask) {
	    if (i >= top) break;
	    array[i] = i;
	    BITSET_ADD(*usedvars, word, mask);
	}
    }



int next_array(int n, int varmax, int array[], bitset *usedvars)
    {
	int i, word;
	bitmask mask;
	int elt;

	/* Search backward for first cell with "room" to be incremented. */
	for (i=n-1;; --i) {
	    if (i<0) return FALSE;	/* no more combinations possible */
	    elt=array[i];
	    word = BITSET_WORD(elt);
	    mask = BITSET_MASK(elt);
	    BITSET_REMOVE(*usedvars, word, mask);
	    (void) NEXT_POSSIBLE_ELEMENT(elt, word, mask);
	    if (next_nonelement(usedvars, &elt, &word, &mask) && elt<varmax)
	      break;
	}
	for (; i<n; ++i) {
	    array[i] = elt;
	    BITSET_ADD(*usedvars, word, mask);
	    elt = 0;
	    word = BITSET_WORD(0);
	    mask = BITSET_MASK(0);
	    if (!next_nonelement(usedvars, &elt, &word, &mask)) return FALSE;
	}
	return TRUE;
    }


void doit(int n, int array[], int varmax, type *f)
    {
	type *result;
#ifdef DEBUGALL
	int i;
	printf("renameArray(");
	printOut(f),
	printf(", %d, [%d", n, array[0]);
	for (i=1; i<n; ++i) printf(",%d", array[i]);
	printf("]) = ");
	fflush(stdout);
#endif /* DEBUGALL */
#ifndef OVERHEAD
	result = renameArray(f, n, array);
#ifdef DEBUGALL
	printOut(result);
	printf("\n");
#endif /* DEBUGALL */
#endif /* !OVERHEAD */
	++opcount;
    }


void dont_doit(int n, int array[], int varmax, type *f)
    {
    }


int main(int argc, char **argv)
    {
	int varmax, size, repetitions;
	int array[MAXVAR];
	bitset set;
	int reps, i;
	type *f;
	millisec clock0, clock1, clock2, clock3;
	float runtime, overhead, rate;
	int test_nodes, overhead_nodes;

	if (argc < 3) {
	    usage(argv[0]);
	    return 20;
	}
	if ((varmax=atoi(argv[2]))<2 || varmax>=MAXVAR) {
	    usage(argv[0]);
	    printf("\n  varmax must be between 2 <= varmax < %d\n", MAXVAR);
	    return 20;
	}
	if ((size=atoi(argv[1]))<0 || size>varmax) {
	    usage(argv[0]);
	    printf("\n  size must be between 0 <= size <= varmax\n");
	    return 20;
	}
	repetitions=(argc>3 ? atoi(argv[3]) : 1);
	if (repetitions <= 0) repetitions = 1;

	for (i=0; i<(size-1)/2; ++i) array[i] = i*2+1;
	f = testing_iff_conj_array(0, (size-1)/2, array);
	for (i=0; i<(size-2)/2; ++i) array[i] = i*2+2;
	f = glb(f, testing_iff_conj_array(size-1, (size-2)/2, array));

	opcount = 0;
	clock0 = milli_time();
	for (reps=repetitions; reps>0; --reps) {
	    init_array(size, array, &set);
	    doit(size, array, varmax, f);
	    while (next_array(size, varmax, array, &set)) {
		doit(size, array, varmax, f);
	    }
	}
	clock1 = milli_time();
	test_nodes = nodes_in_use();
	initRep();

	for (i=0; i<(size-1)/2; ++i) array[i] = i*2+1;
	f = testing_iff_conj_array(0, (size-1)/2, array);
	for (i=0; i<(size-2)/2; ++i) array[i] = i*2+2;
	f = glb(f, testing_iff_conj_array(size-1, (size-2)/2, array));

	clock2 = milli_time();
	for (reps=repetitions; reps>0; --reps) {
	    init_array(size, array, &set);
	    dont_doit(size, array, varmax, f);
	    while (next_array(size, varmax, array, &set)) {
		dont_doit(size, array, varmax, f);
	    }
	}
	clock3 = milli_time();
	overhead_nodes = nodes_in_use();
	runtime = (float)(clock1-clock0)/1000;
	overhead = (float)(clock3-clock2)/1000;
	rate = ((float)opcount)/(runtime-overhead);
	printf("%s %d %d %d:  %.3f - %.3f = %.3f secs, %d ops, %d nodes, %.1f ops/sec\n",
	       argv[0], size, varmax, repetitions,
	       runtime, overhead, (runtime-overhead), opcount,
	       test_nodes-overhead_nodes, rate);
	return 0;
    }
