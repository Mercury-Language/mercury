#include <stdio.h>
#include <stdlib.h>
#ifdef QUINTUS
#include <qio.h>
#endif
#include "bryant.h"
#include "bryantPrint.h"


int print_bryant(node *f, bitset *trues, bitset *falses, int terms);


/* Print out an ROBDD in some readable format.  We display it in disjunctive
 * form.
 */

void printOut(node *f)
    {
	bitset trues, falses;

	if (f == one) {
	    printf("TRUE");
	} else if (f == zero) {
	    printf("FALSE");
	} else {
	    BITSET_CLEAR(trues);
	    BITSET_CLEAR(falses);
	    (void)print_bryant(f, &trues, &falses, 0);
	}
    }


int print_bryant(node *f, bitset *trues, bitset *falses, int terms)
    {
	if (f == one) {
	    bitset all;
	    int var;
	    int word;
	    bitmask mask;
	    char sep = '(';

	    if (terms>0) printf(" ");
	    BITSET_UNION(all, *trues, *falses);
	    FOREACH_ELEMENT(all, var, word, mask) {
		if (BITSET_MEMBER(*trues, word, mask)) {
		    printf("%c%d", sep, var);
		} else {
		    printf("%c~%d", sep, var);
		}
		sep = ' ';
	    }
	    printf(")");
	    ++terms;
	} else if (f != zero) {
	    BITSET_ADD_ELEMENT(*trues, f->value);
	    terms += print_bryant(f->tr, trues, falses, terms);
	    BITSET_TOGGLE_ELEMENT(*trues, f->value);
	    BITSET_ADD_ELEMENT(*falses, f->value);
	    terms += print_bryant(f->fa, trues, falses, terms);
	    BITSET_TOGGLE_ELEMENT(*falses, f->value);
	}
	/* don't do anything for zero terminal */
	return terms;
    }
