/*
** Copyright (C) 2003-2004 Peter Schachte and The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef QUINTUS
#include <qio.h>
#endif
#include "bryant.h"
#include "bryantPrint.h"


int print_bryant(MR_ROBDD_node *f, MR_ROBDD_bitset *trues, MR_ROBDD_bitset *falses, int terms);


/* Print out an ROBDD in some readable format.  We display it in disjunctive
 * form.
 */

void printOut(MR_ROBDD_node *f)
    {
	MR_ROBDD_bitset trues, falses;

	if (f == MR_ROBDD_one) {
	    printf("MR_TRUE");
	} else if (f == MR_ROBDD_zero) {
	    printf("MR_FALSE");
	} else {
	    MR_ROBDD_BITSET_CLEAR(trues);
	    MR_ROBDD_BITSET_CLEAR(falses);
	    (void)print_bryant(f, &trues, &falses, 0);
	}
    }


int print_bryant(MR_ROBDD_node *f, MR_ROBDD_bitset *trues, MR_ROBDD_bitset *falses, int terms)
    {
	if (f == MR_ROBDD_one) {
	    MR_ROBDD_bitset all;
	    int var;
	    int word;
	    MR_ROBDD_bitmask mask;
	    char sep = '(';

	    if (terms>0) printf(" ");
	    MR_ROBDD_BITSET_UNION(all, *trues, *falses);
	    MR_ROBDD_FOREACH_ELEMENT(all, var, word, mask) {
		if (MR_ROBDD_BITSET_MEMBER(*trues, word, mask)) {
		    printf("%c%d", sep, var);
		} else {
		    printf("%c~%d", sep, var);
		}
		sep = ' ';
	    }
	    printf(")");
	    ++terms;
	} else if (f != MR_ROBDD_zero) {
	    MR_ROBDD_BITSET_ADD_ELEMENT(*trues, f->value);
	    terms += print_bryant(f->tr, trues, falses, terms);
	    MR_ROBDD_BITSET_TOGGLE_ELEMENT(*trues, f->value);
	    MR_ROBDD_BITSET_ADD_ELEMENT(*falses, f->value);
	    terms += print_bryant(f->fa, trues, falses, terms);
	    MR_ROBDD_BITSET_TOGGLE_ELEMENT(*falses, f->value);
	}
	/* don't do anything for MR_ROBDD_zero terminal */
	return terms;
    }
