/*****************************************************************
  File     : bryant.h
  RCS      : $Id: bryant.h,v 1.1.2.1 2000-09-21 01:27:39 dgj Exp $
  Author   : Peter Schachte
  Origin   : Sun Jul 30 15:08:53 1995
  Purpose  : header file for users of bryant.c ROBDD package
  Copyright: © 1995 Peter Schachte.  All rights reserved.

*****************************************************************/

#if defined(QUINTUS)
#include <quintus/quintus.h>
#endif
#include <string.h>
#include <assert.h>
#include "var.h"

/*****************************************************************
			  Tunable Parameters
*****************************************************************/

#if defined(AMIGA)

/* number of buckets in unique table */
#define UNIQUE_TABLE_SIZE 4096

/* number of entries in ite computed table */
#define ITE_COMPUTED_TABLE_SIZE 4096

/* number of entries in computed table for binary functions (and, or) */
#define BINARY_CACHE_SIZE 4096

/* allocate bryant nodes this many at a time */
#define POOL_SIZE 4096

#else

/* number of buckets in unique table */
#define UNIQUE_TABLE_SIZE 65537		/* first prime number > 64K */

/* number of entries in ite computed table */
#define COMPUTED_TABLE_SIZE 16411	/* first prime number > 16K */

/* allocate bryant nodes this many at a time */
#define POOL_SIZE 65535
#endif

/* number of bits in an unsigned long, and the log (base 2) of that */
#define BITS_PER_WORD 32
#define LOG_BITS_PER_WORD 5

/* number of bits in an unsigned char, and a bitmask the size of a char */
#define BITS_PER_CHAR 8
#define CHAR_MASK ((1<<BITS_PER_CHAR)-1)

#define INTCAST(p) ((size_t)(p))

/****************************************************************
	       Bryant Graph (ROBDD) Node Data Structure
****************************************************************/

typedef struct graphnode {
	int value;		/* contains name of variable */
	struct graphnode *tr;	/* true (then) child */
	struct graphnode *fa;	/* false (else) child */
	struct graphnode *unique;  /* pointer to next elt in unique table */
} node, type;

/* zero and one are terminal nodes (sinks). */
#define zero         ((node *) 0)
#define one          ((node *) 1)
#define nonterminal ((node *) 2) /* only used by ite_constant */

#define IS_TERMINAL(n) (INTCAST(n) <= 1)

/****************************************************************
			Bit Set Data Structure
****************************************************************/

/* array must put it into a struct because arrays are pretty feeble in C */
typedef struct {
    unsigned long bits[1+(MAXVAR-1)/BITS_PER_WORD];
} bitset;
typedef unsigned long bitmask;

/* Operations to add, remove, toggle, and check for membership of a
 * single element.  The first two of these are used to find the word
 * and mask for use in the rest of them, given an element.  If I
 * trusted the compiler's common subexpression elimination algorithms,
 * I'd code the last 4 of these in terms of the first two, so that
 * they would each take only 2 arguments.  But I don't.
 */
#define BITSET_WORD(elt) ((elt)>>LOG_BITS_PER_WORD)
#define BITSET_MASK(elt) (1 << ((elt)&(BITS_PER_WORD-1)))
#define BITSET_CLEAR(set) memset(&set, 0, sizeof(bitset))
#define BITSET_UNIVERSE(set) memset(&set, ~0, sizeof(bitset))
#define BITSET_MEMBER(set,word,mask) (0!=((set).bits[(word)]&(mask)))
#define BITSET_ADD(set,word,mask) (set).bits[(word)] |= (mask)
#define BITSET_REMOVE(set,word,mask) (set).bits[(word)] &= ~(mask)
#define BITSET_TOGGLE(set,word,mask) (set).bits[(word)] ^= (mask)

/* important bit masks */
#if defined(NO_CHEAP_SHIFT) && BITS_PER_WORD == 32
#define FOLLOWING_BITS(n) following_bits[n]
#define PRECEDING_BITS(n) preceding_bits[n]
#else /* ! NO_CHEAP_SHIFT */
#define FOLLOWING_BITS(n) ((~0UL)<<(n))
#define PRECEDING_BITS(n) ((~0UL)>>(BITS_PER_WORD-1-(n)))
#endif /* NO_CHEAP_SHIFT */

#define BITSET_IS_MEMBER(set,n) \
  BITSET_MEMBER(set, BITSET_WORD(n), BITSET_MASK(n))
#define BITSET_ADD_ELEMENT(set,n) \
  BITSET_ADD(set, BITSET_WORD(n), BITSET_MASK(n))
#define BITSET_REMOVE_ELEMENT(set,n) \
  BITSET_REMOVE(set, BITSET_WORD(n), BITSET_MASK(n))
#define BITSET_TOGGLE_ELEMENT(set,n) \
  BITSET_TOGGLE(set, BITSET_WORD(n), BITSET_MASK(n))

/* Macros for operations on sets.  These are destructive: the first
 * argument is modified to hold the result.  The i parameter is just a
 * usable integer variable that will be destroyed by the macro.  These
 * should probably be __inline functions, but I don't trust that,
 * either.  Portability, you know.
 */
#define BITSET_INTERSECTION(set,set1,set2)				\
  do { int i; for (i=0; i<=((MAXVAR-1)/BITS_PER_WORD); ++i)	\
	 (set).bits[i] = (set1).bits[i] & (set2).bits[i];} while (0)
#define BITSET_DIFFERENCE(set,set1,set2)				\
  do { int i; for (i=0; i<=((MAXVAR-1)/BITS_PER_WORD); ++i)	\
	 (set).bits[i] = (set1).bits[i] & ~((set2).bits[i]); } while (0)
#define BITSET_UNION(set,set1,set2)					\
  do { int i; for (i=0; i<=((MAXVAR-1)/BITS_PER_WORD); ++i) 	\
	 (set).bits[i] = (set1).bits[i] | (set2).bits[i]; } while (0)
#define BITSET_EXCLUSIVE_UNION(set,set1,set2)			\
  do { int i; for (i=0; i<=((MAXVAR-1)/BITS_PER_WORD); ++i)	\
	 (set).bits[i] = (set1).bits[i] ^ (set2).bits[i]; } while (0)

#define BITSET_EQUAL(set1, set2) bitset_equal(&set1, &set2)
#define BITSET_DISJOINT(set1, set2) bitset_disjoint(&set1, &set2)
#define BITSET_SUBSET(set1, set2) bitset_subset(&set1, &set2)
#define BITSET_EMPTY(set) bitset_empty(&set)


/* Successor and predecessor for possible set elements.  These are
 * expressions that are false if there are no more possible elements.
 */
#define NEXT_POSSIBLE_ELEMENT(var,word,mask) \
  (++var<MAXVAR && ((mask<<=1) || (mask=1,++word)))
#define PREV_POSSIBLE_ELEMENT(var,word,mask) \
  (--var>=0 && ((mask>>=1) || (mask=1<<(BITS_PER_WORD-1),--word)))


/* Enumerating sets.  Use these like for loops:  follow the macro call with
 * a statement (or an open brace, some statements, and a close brace).  The
 * first three iterate from low to high, the last three from high to low.
 */
#define FOREACH_POSSIBLE_ELEMENT(var,word,mask) \
  for (var=0,word=0,mask=1; var<MAXVAR;		\
       (void) NEXT_POSSIBLE_ELEMENT(var,word,mask))
#define FOREACH_ELEMENT(set,var,word,mask) \
  for (var=0,word=0,mask=1; next_element(&set,&var,&word,&mask); \
       (void) NEXT_POSSIBLE_ELEMENT(var,word,mask))
#define FOREACH_NONELEMENT(set,var,word,mask) \
  for (var=0,word=0,mask=1; next_nonelement(&set,&var,&word,&mask); \
       (void) NEXT_POSSIBLE_ELEMENT(var,word,mask))

#define REV_FOREACH_POSSIBLE_ELEMENT(var,word,mask) \
  for (var=MAXVAR-1,word=((MAXVAR-1)/BITS_PER_WORD),mask=1<<(BITS_PER_WORD-1); \
       var>0; (void) PREV_POSSIBLE_ELEMENT(var,word,mask))
#define REV_FOREACH_ELEMENT(set,var,word,mask) \
  for (var=MAXVAR-1,word=((MAXVAR-1)/BITS_PER_WORD),mask=1<<(BITS_PER_WORD-1); \
       prev_element(&set,&var,&word,&mask); \
       (void) PREV_POSSIBLE_ELEMENT(var,word,mask))
#define REV_FOREACH_NONELEMENT(set,var,word,mask) \
  for (var=MAXVAR-1,word=((MAXVAR-1)/BITS_PER_WORD),mask=1<<(BITS_PER_WORD-1); \
       prev_nonelement(&set,&var,&word,&mask); \
       (void) PREV_POSSIBLE_ELEMENT(var,word,mask))


/*****************************************************************
			  Other Definitions
*****************************************************************/


#define TRUE 1
#define FALSE 0

/* sneaky trick to make NEW the default */
#if !defined(USE_RGLB) \
      && !defined(USE_THRESH) \
      && !defined(OLD) \
      && !defined(NAIVE) \
      && !defined(NEW)
#define NEW
#endif

#if defined(NEW)
#define USE_RGLB
#endif /* NEW */

#if defined(USE_RGLB)
#define USE_THRESH
#endif /* USE_RGLB */

#if defined(USE_THRESH)
#define OLD
#if !defined(NEW)
#define USE_ITE_CONSTANT /* for var_entailed */
#endif /* !NEW */
#endif /* USE_THRESH */

#if defined(NEW)
#define WHICH "NEW"
#elif defined(USE_RGLB)
#define WHICH "RGLB"
#elif defined(USE_THRESH)
#define WHICH "THRESH"
#elif defined(OLD)
#define WHICH "OLD"
#elif defined(NAIVE)
#define WHICH "NAIVE"
#else
#error "must define one of NEW, USE_RGLB, USE_THRESH, OLD, or NAIVE."
#endif


/*****************************************************************
				 Public Data
*****************************************************************/

extern unsigned char first_one_bit[256];
extern unsigned char last_one_bit[256];

#if defined(NO_CHEAP_SHIFT) && BITS_PER_WORD == 32
extern bitmask following_bits[BITS_PER_WORD];
extern bitmask preceding_bits[BITS_PER_WORD];
#endif

/*****************************************************************
			      Prototypes
*****************************************************************/

/* this must be called before any other function in this file */
extern void initRep(void);

/* this should be called when you're done calling functions in this file */
/* to clean up memory used by ROBDDs.  After calling this, you must call */
/* InitRep() again before calling any other functions in this file */
extern void concludeRep(void);


/* the basic make a node or return an existing node operation */
extern node *make_node(int var, node *tr, node *fa);

/* returns one (the Boolean function true) */
extern node *trueVar(void);
/* returns zero (the Boolean function false) */
extern node *falseVar(void);
/* returns var, as an ROBDD.  */
extern node *variableRep(int var);


/* returns a \wedge b */
extern node *glb(node *a, node *b);
/* returns a \vee b */
extern node *lub(node *a, node *b);
/* returns a \rightarrow b */
extern node *implies(node *a, node *b);

/* returns \exists c . a */
/* extern node *restrict(int c, node *a); */

/* returns \bigglb_{0 \leq i \leq n} array[i] */
extern node *glb_array(int n, int arr[]);

/* returns a with variable o renamed to n */
extern node *changename(int o, int n, node *a);
/* returns a with variable 1 renamed to v1, 2 renamed to v2, ... n renamed */
/* to v_n.  Here n is the number of variables to rename */
extern node *renameList(node *a, int n, int v1, int v2, int v3, int v4, int v5,
		 int v6, int v7, int v8, int v9, int v10, int v11, int v12,
		 int v13, int v14, int v15, int v16);
/* returns a with variable v1 renamed to 1, v2 renamed to 2, ... v_n renamed */
/* to n.  Here n is the number of variables to rename */
extern node *reverseRenameList(node *a, int n, int v1, int v2, int v3, int v4,
			int v5, int v6, int v7, int v8, int v9, int v10,
			int v11, int v12, int v13, int v14, int v15, int v16);
/* returns a with variable 0 renamed to mapping[0], 1 renamed to */
/* mapping[1], ... count renamed to mapping[count]. */
extern node *renameArray(node *in, int count, int mappping[]);
/* returns a with variable mapping[0] renamed to 0, mapping[1] renamed to */
/* 1, ... mapping[count] renamed to count. */
extern node *reverseRenameArray(node *in, int count, int rev_mappping[]);

/* returns v0 \leftrightarrow \bigwedge_{i=0}^{n} arr[i] */
extern node *iff_conj_array(int v0, int n, int arr[]);
/* returns v0 \leftrightarrow \bigwedge_{i=0}^{n} v_i */
extern node *iff_conj(int v0, int n, int v1, int v2, int v3, int v4, int v5,
		      int v6, int v7, int v8, int v9, int v10, int v11,
		      int v12, int v13, int v14, int v15, int v16);
/* returns non-zero iff f entails variable number var */
extern int var_entailed(node *f, int var);

/* Finds the smallest n such that n \in set and n \geq *var.  word and */
/* mask must be as set by BITSET_WORD(*var) and BITSET_MASK(*var), */
/* respectively.  The resulting n is placed in *var, and *word and *mask */
/* are updated correspondingly.  Returns TRUE iff there is such an n. */
int next_element(bitset *set, int *var, int *word, bitmask *mask);

/* Finds the largest n such that n \in set and n \leq *var.  word and */
/* mask must be as set by BITSET_WORD(*var) and BITSET_MASK(*var), */
/* respectively.  The resulting n is placed in *var, and *word and *mask */
/* are updated correspondingly.  Returns TRUE iff there is such an n. */
int prev_element(bitset *set, int *var, int *word, bitmask *mask);

/* Finds the smallest n such that n \not \in set and n \geq *var.  word and */
/* mask must be as set by BITSET_WORD(*var) and BITSET_MASK(*var), */
/* respectively.  The resulting n is placed in *var, and *word and *mask */
/* are updated correspondingly.  Returns TRUE iff there is such an n. */
int next_nonelement(bitset *set, int *var, int *word, bitmask *mask);

/* Finds the largest n such that n \not \in set and n \leq *var.  word and */
/* mask must be as set by BITSET_WORD(*var) and BITSET_MASK(*var), */
/* respectively.  The resulting n is placed in *var, and *word and *mask */
/* are updated correspondingly.  Returns TRUE iff there is such an n. */
int prev_nonelement(bitset *set, int *var, int *word, bitmask *mask);


#if !defined(USE_THRESH) && !defined(RESTRICT_SET)

/* returns a with all variables lo \leq v \leq hi restricted away */
extern node *restrictThresh(int lo, int hi, node *a);

/* returns f \wedge g with all variables lo \leq v \leq hi restricted away */
extern node *restricted_glb(int lo, int hi, node *f, node *g);

/* computes g = f with variable 0 renamed to mapping[0], 1 renamed to */
/* mapping[1], ... count renamed to mapping[count].  Returns context */
/* \wedge g with all variables lo \leq v \leq hi restricted away */
extern node *abstract_exit(node *context, node *f, int count, int mapping[],
		    int lo, int hi);
/* returns f \wedge (v0 \leftrightarrow \bigwedge_{i=0}^{n} arr[i]), */
/* with all variables lo \leq v \leq hi restricted away */
extern node *abstract_unify(node *f, int v0, int n, int arr[], int lo, int hi);
#else /* USE_THRESH || RESTRICT_SET */

/* returns a with all variables lo \leq v \leq hi restricted away */
extern node *restrictThresh(int c,node *a);

/* returns f \wedge g with all variables v \geq c restricted away */
extern node *restricted_glb(int c, node *f, node *g);

/* computes g = f with variable 0 renamed to mapping[0], 1 renamed to */
/* mapping[1], ... count renamed to mapping[count].  Returns context */
/* \wedge g with all variables v \geq thresh restricted away */
extern node *abstract_exit(node *context, node *f, int count, int mapping[],
		    int thresh);

/* returns f \wedge (v0 \leftrightarrow \bigwedge_{i=0}^{n} arr[i]), */
/* with all variables v \eq thresh restricted away */
extern node *abstract_unify(node *f, int v0, int n, int arr[], int thresh);
#endif /* !OLD || USE_THRESH */

#if !defined(NEW)
/* returns the set of all v entailed by f where v \leq topvar */
extern bitset *vars_entailed(node *f, int topvar);
#else /* NEW */
/* returns the set of all v entailed by f */
extern bitset *vars_entailed(node *f);
#endif /* NEW */

/* return the initial set sharing representation for n variables */
extern node *init_set_sharing(int n);
/* computes the set sharing upward closure of f */
extern node *upclose(node *f);
/* performs Langen's bin operation, used for set sharing analysis */
extern node *bin(node *f, node *g);

/* prints out the bryant graph a */
extern void printOut(node *a);

/* for profiling purposes:  return the number of ROBDD nodes in use */
extern int nodes_in_use(void);
/* for profiling only:  the same as iff_conj_array(), but as efficient */
/* as possible, whatever variables are #defined */
node *testing_iff_conj_array(int v0, int n, int arr[]);


/* These are not really useful for ROBDDs but are needed for other */
/* representations of Boolean functions. */
/* free n */
extern void free_rep(node *n);
/* free n if it doesn't share with m */
extern void free_rep_if_diff(node *n, node *m);
/* returns a copy of a.  For ROBDDs this is just a */
extern node *copy(node *a);
/* returns non-zero iff a = b; for ROBDDs, use a==b instead */
extern int equiv(node *a, node *b);

/* for a more efficient interface from Quintus Prolog. */
#if defined(QUINTUS)
extern node *renameTerm(node *in, QP_term_ref term);
extern node *reverseRenameTerm(node *in, QP_term_ref term);
#endif /* QUINTUS */
