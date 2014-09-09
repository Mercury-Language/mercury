/*
** vim: ts=4 sw=4 et ft=c
*/
/*
** Copyright (C) 1995, 2001-2005 Peter Schachte 
** Copyright (C) 1995, 2001-2005, 2010 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*****************************************************************
  File     : bryant.h
  Author   : Peter Schachte
  Origin   : Sun Jul 30 15:08:53 1995
  Purpose  : header file for users of bryant.c ROBDD package
*****************************************************************/

#ifndef MERCURY_BRYANT_H
#define MERCURY_BRYANT_H

#if defined(QUINTUS)
  #include <quintus/quintus.h>
#endif
#include <string.h>
#include <assert.h>
#include "var.h"
#if defined(CONSERVATIVE_GC)
  #define GC_I_HIDE_POINTERS
  #if defined(MR_HGC)
    #include "mercury_hgc.h"
  #else
  #include "gc.h"
  #endif
  #define MR_ROBDD_BRYANT_CONSERVATIVE_GC
  #define MR_ROBDD_HIDE_POINTER(p)      HIDE_POINTER(p)
  #define MR_ROBDD_REVEAL_POINTER(p)    REVEAL_POINTER(p)
#else
  #define MR_ROBDD_HIDE_POINTER(p)      (p)
  #define MR_ROBDD_REVEAL_POINTER(p)    (p)
#endif

#include "robdd_conf.h"

/*****************************************************************
              Tunable Parameters
*****************************************************************/

#if defined(AMIGA)

  /* number of buckets in unique table */
  #define MR_ROBDD_UNIQUE_TABLE_SIZE 4096

  /* number of entries in MR_ROBDD_ite computed table */
  #define MR_ROBDD_ITE_COMPUTED_TABLE_SIZE 4096

  /* number of entries in computed table for binary functions (and, or) */
  #define MR_ROBDD_BINARY_CACHE_SIZE 4096

  /* allocate bryant nodes this many at a time */
  #define MR_ROBDD_POOL_SIZE 4096

#else

  /* number of buckets in unique table */
  #define MR_ROBDD_UNIQUE_TABLE_SIZE 65537      /* first prime number > 64K */

  /* number of entries in MR_ROBDD_ite computed table */
  #define MR_ROBDD_COMPUTED_TABLE_SIZE 16411    /* first prime number > 16K */

  /* allocate bryant nodes this many at a time */
  #define MR_ROBDD_POOL_SIZE 65535

#endif

/* number of bits in an unsigned char, and a MR_ROBDD_bitmask the size of a char */
#define MR_ROBDD_BITS_PER_CHAR 8
#define MR_ROBDD_CHAR_MASK ((1<<MR_ROBDD_BITS_PER_CHAR)-1)

#define MR_ROBDD_INTCAST(p) ((size_t)(p))

/****************************************************************
           Bryant Graph (ROBDD) Node Data Structure
****************************************************************/

typedef struct MR_ROBDD_graphnode {
    MR_ROBDD_int value;     /* contains name of variable */
    struct MR_ROBDD_graphnode *tr;  /* true (then) child */
    struct MR_ROBDD_graphnode *fa;  /* false (else) child */
#if defined(MR_ROBDD_BRYANT_CONSERVATIVE_GC)
    GC_hidden_pointer unique; /* pointer to next elt in unique table */
    GC_hidden_pointer uprev;  /* pointer to the prev elt in unique table */
#else
    struct MR_ROBDD_graphnode *unique;  /* pointer to next elt in unique table */
#endif
} MR_ROBDD_node, MR_ROBDD_type;

#if defined(MR_ROBDD_BRYANT_CONSERVATIVE_GC)
  typedef GC_hidden_pointer MR_ROBDD_BRYANT_hidden_node_pointer;
#else
  typedef MR_ROBDD_node *MR_ROBDD_BRYANT_hidden_node_pointer;
#endif

/* MR_ROBDD_zero and MR_ROBDD_one are terminal nodes (sinks). */
#define MR_ROBDD_zero         ((MR_ROBDD_node *) 0)
#define MR_ROBDD_one          ((MR_ROBDD_node *) 1)
#define MR_ROBDD_nonterminal  ((MR_ROBDD_node *) 2) /* only used by MR_ROBDD_ite_constant */

#define MR_ROBDD_IS_TERMINAL(n) (MR_ROBDD_INTCAST(n) <= 1)

/****************************************************************
            Bit Set Data Structure
****************************************************************/

/* array must put it into a struct because arrays are pretty feeble in C */
typedef struct {
    MR_ROBDD_uint bits[1+(MR_ROBDD_MAXVAR-1)/MR_ROBDD_BITS_PER_WORD];
} MR_ROBDD_bitset;
typedef MR_ROBDD_uint MR_ROBDD_bitmask;

/*
 * Operations to add, remove, toggle, and check for membership of a
 * single element.  The first two of these are used to find the word
 * and mask for use in the rest of them, given an element.  If I
 * trusted the compiler's common subexpression elimination algorithms,
 * I'd code the last 4 of these in terms of the first two, so that
 * they would each take only 2 arguments.  But I don't.
 */
#define MR_ROBDD_BITSET_WORD(elt) ((elt)>>MR_ROBDD_LOG_BITS_PER_WORD)
#define MR_ROBDD_BITSET_MASK(elt) (1 << ((elt)&(MR_ROBDD_BITS_PER_WORD-1)))
#define MR_ROBDD_BITSET_CLEAR(set) memset(&set, 0, sizeof(MR_ROBDD_bitset))
#define MR_ROBDD_BITSET_UNIVERSE(set) memset(&set, ~0, sizeof(MR_ROBDD_bitset))
#define MR_ROBDD_BITSET_MEMBER(set,word,mask) (0!=((set).bits[(word)]&(mask)))
#define MR_ROBDD_BITSET_ADD(set,word,mask) (set).bits[(word)] |= (mask)
#define MR_ROBDD_BITSET_REMOVE(set,word,mask) (set).bits[(word)] &= ~(mask)
#define MR_ROBDD_BITSET_TOGGLE(set,word,mask) (set).bits[(word)] ^= (mask)

/* important bit masks */
#if defined(MR_ROBDD_NO_CHEAP_SHIFT) && MR_ROBDD_BITS_PER_WORD == 32
  #define MR_ROBDD_FOLLOWING_BITS(n) MR_ROBDD_following_bits[n]
  #define MR_ROBDD_PRECEDING_BITS(n) MR_ROBDD_preceding_bits[n]
#else /* ! MR_ROBDD_NO_CHEAP_SHIFT */
  #define MR_ROBDD_FOLLOWING_BITS(n) ((~0UL)<<(n))
  #define MR_ROBDD_PRECEDING_BITS(n) ((~0UL)>>(MR_ROBDD_BITS_PER_WORD-1-(n)))
#endif /* MR_ROBDD_NO_CHEAP_SHIFT */

#define MR_ROBDD_BITSET_IS_MEMBER(set,n) \
  MR_ROBDD_BITSET_MEMBER(set, MR_ROBDD_BITSET_WORD(n), MR_ROBDD_BITSET_MASK(n))
#define MR_ROBDD_BITSET_ADD_ELEMENT(set,n) \
  MR_ROBDD_BITSET_ADD(set, MR_ROBDD_BITSET_WORD(n), MR_ROBDD_BITSET_MASK(n))
#define MR_ROBDD_BITSET_REMOVE_ELEMENT(set,n) \
  MR_ROBDD_BITSET_REMOVE(set, MR_ROBDD_BITSET_WORD(n), MR_ROBDD_BITSET_MASK(n))
#define MR_ROBDD_BITSET_TOGGLE_ELEMENT(set,n) \
  MR_ROBDD_BITSET_TOGGLE(set, MR_ROBDD_BITSET_WORD(n), MR_ROBDD_BITSET_MASK(n))

/*
 * Macros for operations on sets.  These are destructive: the first
 * argument is modified to hold the result.  The i parameter is just a
 * usable integer variable that will be destroyed by the macro.  These
 * should probably be __inline functions, but I don't trust that,
 * either.  Portability, you know.
 */
#define MR_ROBDD_BITSET_INTERSECTION(set,set1,set2)                         \
    do {                                                                    \
        MR_ROBDD_int i;                                                     \
        for (i=0; i<=((MR_ROBDD_MAXVAR-1)/MR_ROBDD_BITS_PER_WORD); ++i)     \
            (set).bits[i] = (set1).bits[i] & (set2).bits[i];                \
    } while (0)
#define MR_ROBDD_BITSET_DIFFERENCE(set,set1,set2)                           \
    do {                                                                    \
        MR_ROBDD_int i;                                                     \
        for (i=0; i<=((MR_ROBDD_MAXVAR-1)/MR_ROBDD_BITS_PER_WORD); ++i)     \
            (set).bits[i] = (set1).bits[i] & ~((set2).bits[i]);             \
    } while (0)
#define MR_ROBDD_BITSET_UNION(set,set1,set2)                                \
    do {                                                                    \
        MR_ROBDD_int i;                                                     \
        for (i=0; i<=((MR_ROBDD_MAXVAR-1)/MR_ROBDD_BITS_PER_WORD); ++i)     \
            (set).bits[i] = (set1).bits[i] | (set2).bits[i];                \
    } while (0)
#define MR_ROBDD_BITSET_EXCLUSIVE_UNION(set,set1,set2)                      \
    do {                                                                    \
        MR_ROBDD_int i;                                                     \
        for (i=0; i<=((MR_ROBDD_MAXVAR-1)/MR_ROBDD_BITS_PER_WORD); ++i)     \
            (set).bits[i] = (set1).bits[i] ^ (set2).bits[i];                \
    } while (0)

#define MR_ROBDD_BITSET_EQUAL(set1, set2)       \
    MR_ROBDD_bitset_equal(&set1, &set2)
#define MR_ROBDD_BITSET_DISJOINT(set1, set2)    \
    MR_ROBDD_bitset_disjoint(&set1, &set2)
#define MR_ROBDD_BITSET_SUBSET(set1, set2)      \
    MR_ROBDD_bitset_subset(&set1, &set2)
#define MR_ROBDD_BITSET_EMPTY(set)              \
    MR_ROBDD_bitset_empty(&set)

/*
 * Successor and predecessor for possible set elements.  These are
 * expressions that are false if there are no more possible elements.
 */
#define MR_ROBDD_NEXT_POSSIBLE_ELEMENT(var,word,mask) \
  (++var<MR_ROBDD_MAXVAR && ((mask<<=1) || (mask=1,++word)))
#define MR_ROBDD_PREV_POSSIBLE_ELEMENT(var,word,mask) \
  (--var>=0 && ((mask>>=1) || (mask=1<<(MR_ROBDD_BITS_PER_WORD-1),--word)))

/*
 * Enumerating sets.  Use these like for loops:  follow the macro call with
 * a statement (or an open brace, some statements, and a close brace).  The
 * first three iterate from low to high, the last three from high to low.
 */
#define MR_ROBDD_FOREACH_POSSIBLE_ELEMENT(var,word,mask) \
  for (var=0,word=0,mask=1; var<MR_ROBDD_MAXVAR;        \
       (void) MR_ROBDD_NEXT_POSSIBLE_ELEMENT(var,word,mask))
#define MR_ROBDD_FOREACH_ELEMENT(set,var,word,mask) \
  for (var=0,word=0,mask=1; MR_ROBDD_next_element(&set,&var,&word,&mask); \
       (void) MR_ROBDD_NEXT_POSSIBLE_ELEMENT(var,word,mask))
#define MR_ROBDD_FOREACH_NONELEMENT(set,var,word,mask) \
  for (var=0,word=0,mask=1; MR_ROBDD_next_nonelement(&set,&var,&word,&mask); \
       (void) MR_ROBDD_NEXT_POSSIBLE_ELEMENT(var,word,mask))

#define MR_ROBDD_REV_FOREACH_POSSIBLE_ELEMENT(var,word,mask) \
  for (var=MR_ROBDD_MAXVAR-1,word=((MR_ROBDD_MAXVAR-1)/MR_ROBDD_BITS_PER_WORD),mask=1<<(MR_ROBDD_BITS_PER_WORD-1); \
       var>0; (void) MR_ROBDD_PREV_POSSIBLE_ELEMENT(var,word,mask))
#define MR_ROBDD_REV_FOREACH_ELEMENT(set,var,word,mask) \
  for (var=MR_ROBDD_MAXVAR-1,word=((MR_ROBDD_MAXVAR-1)/MR_ROBDD_BITS_PER_WORD),mask=1<<(MR_ROBDD_BITS_PER_WORD-1); \
       MR_ROBDD_prev_element(&set,&var,&word,&mask); \
       (void) MR_ROBDD_PREV_POSSIBLE_ELEMENT(var,word,mask))
#define MR_ROBDD_REV_FOREACH_NONELEMENT(set,var,word,mask) \
  for (var=MR_ROBDD_MAXVAR-1,word=((MR_ROBDD_MAXVAR-1)/MR_ROBDD_BITS_PER_WORD),mask=1<<(MR_ROBDD_BITS_PER_WORD-1); \
       MR_ROBDD_prev_nonelement(&set,&var,&word,&mask); \
       (void) MR_ROBDD_PREV_POSSIBLE_ELEMENT(var,word,mask))

/*****************************************************************
              Other Definitions
*****************************************************************/

#ifndef MR_TRUE
  #define MR_TRUE 1
#endif
#ifndef MR_FALSE
  #define MR_FALSE 0
#endif

/* sneaky trick to make MR_ROBDD_NEW the default */
#if !defined(MR_ROBDD_USE_RGLB) \
      && !defined(MR_ROBDD_USE_THRESH) \
      && !defined(MR_ROBDD_OLD) \
      && !defined(MR_ROBDD_NAIVE) \
      && !defined(MR_ROBDD_NEW)
  #define MR_ROBDD_NEW
#endif

#if defined(MR_ROBDD_NEW)
  #define MR_ROBDD_USE_RGLB
#endif /* MR_ROBDD_NEW */

#if defined(MR_ROBDD_USE_RGLB)
  #define MR_ROBDD_USE_THRESH
#endif /* MR_ROBDD_USE_RGLB */

#if defined(MR_ROBDD_USE_THRESH)
  #define MR_ROBDD_OLD
  #if !defined(MR_ROBDD_NEW)
    #define MR_ROBDD_USE_ITE_CONSTANT /* for MR_ROBDD_var_entailed */
  #endif /* !MR_ROBDD_NEW */
#endif /* MR_ROBDD_USE_THRESH */

#if defined(MR_ROBDD_NEW)
  #define MR_ROBDD_WHICH "MR_ROBDD_NEW"
#elif defined(MR_ROBDD_USE_RGLB)
  #define MR_ROBDD_WHICH "RGLB"
#elif defined(MR_ROBDD_USE_THRESH)
  #define MR_ROBDD_WHICH "THRESH"
#elif defined(MR_ROBDD_OLD)
  #define MR_ROBDD_WHICH "MR_ROBDD_OLD"
#elif defined(MR_ROBDD_NAIVE)
  #define MR_ROBDD_WHICH "MR_ROBDD_NAIVE"
#else
  #error "must define MR_ROBDD_one of MR_ROBDD_NEW, MR_ROBDD_USE_RGLB, MR_ROBDD_USE_THRESH, MR_ROBDD_OLD, or MR_ROBDD_NAIVE."
#endif

/*****************************************************************
                 Public Data
*****************************************************************/

extern unsigned char MR_ROBDD_first_one_bit[256];
extern unsigned char MR_ROBDD_last_one_bit[256];

#if defined(MR_ROBDD_NO_CHEAP_SHIFT) && MR_ROBDD_BITS_PER_WORD == 32
  extern MR_ROBDD_bitmask MR_ROBDD_following_bits[MR_ROBDD_BITS_PER_WORD];
  extern MR_ROBDD_bitmask MR_ROBDD_preceding_bits[MR_ROBDD_BITS_PER_WORD];
#endif

/*****************************************************************
                  Prototypes
*****************************************************************/

/* this must be called before any other function in this file */
extern void MR_ROBDD_initRep(void);

extern void MR_ROBDD_init_caches(void);

/* this should be called when you're done calling functions in this file */
/* to clean up memory used by ROBDDs.  After calling this, you must call */
/* InitRep() again before calling any other functions in this file */
extern void MR_ROBDD_concludeRep(void);

/* the basic make a MR_ROBDD_node or return an existing MR_ROBDD_node operation */
extern MR_ROBDD_node *  MR_ROBDD_make_node(MR_ROBDD_int var, MR_ROBDD_node *tr,
                            MR_ROBDD_node *fa);

/* returns MR_ROBDD_one (the Boolean function true) */
extern MR_ROBDD_node    *MR_ROBDD_trueVar(void);
/* returns MR_ROBDD_zero (the Boolean function false) */
extern MR_ROBDD_node    *MR_ROBDD_falseVar(void);
/* returns var, as an ROBDD.  */
extern MR_ROBDD_node    *MR_ROBDD_variableRep(MR_ROBDD_int var);

/* if then else algorithm */
extern MR_ROBDD_node    *MR_ROBDD_ite(MR_ROBDD_node *f, MR_ROBDD_node *g,
                            MR_ROBDD_node *h);

/*
** This is sort of an "approximate MR_ROBDD_ite()."
** It returns MR_ROBDD_zero or MR_ROBDD_one if
** that's what MR_ROBDD_ite() would do.  Otherwise it just returns the
** pseudo-MR_ROBDD_node `MR_ROBDD_nonterminal' or some real MR_ROBDD_node.
** In any case, it does not create any new nodes.
*/
#ifdef MR_ROBDD_USE_ITE_CONSTANT
  extern MR_ROBDD_node  *MR_ROBDD_ite_constant(MR_ROBDD_node *f,
                            MR_ROBDD_node *g, MR_ROBDD_node *h);
#endif

extern MR_ROBDD_node    *MR_ROBDD_ite_var(MR_ROBDD_int f, MR_ROBDD_node *g,
                            MR_ROBDD_node *h);

/* returns a \wedge b */
extern MR_ROBDD_node    *MR_ROBDD_glb(MR_ROBDD_node *a, MR_ROBDD_node *b);
/* returns a \vee b */
extern MR_ROBDD_node    *MR_ROBDD_lub(MR_ROBDD_node *a, MR_ROBDD_node *b);
/* returns a \rightarrow b */
extern MR_ROBDD_node    *MR_ROBDD_implies(MR_ROBDD_node *a, MR_ROBDD_node *b);

/* returns \exists c . a */
extern MR_ROBDD_node    *MR_ROBDD_restrict(MR_ROBDD_int c, MR_ROBDD_node *f);

/* returns \bigglb_{0 \leq i \leq n} array[i] */
extern MR_ROBDD_node    *MR_ROBDD_glb_array(MR_ROBDD_int n,
                            MR_ROBDD_int arr[]);

/* returns a with variable o renamed to n */
extern MR_ROBDD_node    *changename(MR_ROBDD_int o, MR_ROBDD_int n,
                            MR_ROBDD_node *a);
/* returns a with variable 1 renamed to v1, 2 renamed to v2, ... n renamed */
/* to v_n.  Here n is the number of variables to rename */
extern MR_ROBDD_node    *renameList(MR_ROBDD_node *a, MR_ROBDD_int n,
                            MR_ROBDD_int v1, MR_ROBDD_int v2,
                            MR_ROBDD_int v3, MR_ROBDD_int v4,
                            MR_ROBDD_int v5, MR_ROBDD_int v6,
                            MR_ROBDD_int v7, MR_ROBDD_int v8,
                            MR_ROBDD_int v9, MR_ROBDD_int v10,
                            MR_ROBDD_int v11, MR_ROBDD_int v12,
                            MR_ROBDD_int v13, MR_ROBDD_int v14,
                            MR_ROBDD_int v15, MR_ROBDD_int v16);
/* returns a with variable v1 renamed to 1, v2 renamed to 2, ... v_n renamed */
/* to n.  Here n is the number of variables to rename */
extern MR_ROBDD_node    *reverseRenameList(MR_ROBDD_node *a, MR_ROBDD_int n,
                            MR_ROBDD_int v1, MR_ROBDD_int v2,
                            MR_ROBDD_int v3, MR_ROBDD_int v4,
                            MR_ROBDD_int v5, MR_ROBDD_int v6,
                            MR_ROBDD_int v7, MR_ROBDD_int v8,
                            MR_ROBDD_int v9, MR_ROBDD_int v10,
                            MR_ROBDD_int v11, MR_ROBDD_int v12,
                            MR_ROBDD_int v13, MR_ROBDD_int v14,
                            MR_ROBDD_int v15, MR_ROBDD_int v16);
/* returns a with variable 0 renamed to mapping[0], 1 renamed to */
/* mapping[1], ... count renamed to mapping[count]. */
extern MR_ROBDD_node    *MR_ROBDD_renameArray(MR_ROBDD_node *in,
                            MR_ROBDD_int count, MR_ROBDD_int mappping[]);
/* returns a with variable mapping[0] renamed to 0, mapping[1] renamed to */
/* 1, ... mapping[count] renamed to count. */
extern MR_ROBDD_node    *MR_ROBDD_reverseRenameArray(MR_ROBDD_node *in,
                            MR_ROBDD_int count, MR_ROBDD_int rev_mappping[]);

/* returns v0 \leftrightarrow \bigwedge_{i=0}^{n} arr[i] */
extern MR_ROBDD_node    *MR_ROBDD_iff_conj_array(MR_ROBDD_int v0,
                            MR_ROBDD_int n, MR_ROBDD_int arr[]);
/* returns v0 \leftrightarrow \bigwedge_{i=0}^{n} v_i */
extern MR_ROBDD_node    *iff_conj(MR_ROBDD_int v0, MR_ROBDD_int n,
                            MR_ROBDD_int v1, MR_ROBDD_int v2,
                            MR_ROBDD_int v3, MR_ROBDD_int v4,
                            MR_ROBDD_int v5, MR_ROBDD_int v6,
                            MR_ROBDD_int v7, MR_ROBDD_int v8,
                            MR_ROBDD_int v9, MR_ROBDD_int v10,
                            MR_ROBDD_int v11, MR_ROBDD_int v12,
                            MR_ROBDD_int v13, MR_ROBDD_int v14,
                            MR_ROBDD_int v15, MR_ROBDD_int v16);
/* returns non-MR_ROBDD_zero iff f entails variable number var */
extern int              MR_ROBDD_var_entailed(MR_ROBDD_node *f,
                            MR_ROBDD_int var);

/* Finds the smallest n such that n \in set and n \geq *var.  word and */
/* mask must be as set by MR_ROBDD_BITSET_WORD(*var) and MR_ROBDD_BITSET_MASK(*var), */
/* respectively.  The resulting n is placed in *var, and *word and *mask */
/* are updated correspondingly.  Returns MR_TRUE iff there is such an n. */
extern MR_ROBDD_int     MR_ROBDD_next_element(MR_ROBDD_bitset *set,
                            MR_ROBDD_int *var, MR_ROBDD_int *word,
                            MR_ROBDD_bitmask *mask);

/* Finds the largest n such that n \in set and n \leq *var.  word and */
/* mask must be as set by MR_ROBDD_BITSET_WORD(*var) and MR_ROBDD_BITSET_MASK(*var), */
/* respectively.  The resulting n is placed in *var, and *word and *mask */
/* are updated correspondingly.  Returns MR_TRUE iff there is such an n. */
extern MR_ROBDD_int     MR_ROBDD_prev_element(MR_ROBDD_bitset *set,
                            MR_ROBDD_int *var, MR_ROBDD_int *word,
                            MR_ROBDD_bitmask *mask);

/* Finds the smallest n such that n \not \in set and n \geq *var.  word and */
/* mask must be as set by MR_ROBDD_BITSET_WORD(*var) and MR_ROBDD_BITSET_MASK(*var), */
/* respectively.  The resulting n is placed in *var, and *word and *mask */
/* are updated correspondingly.  Returns MR_TRUE iff there is such an n. */
extern MR_ROBDD_int     MR_ROBDD_next_nonelement(MR_ROBDD_bitset *set,
                            MR_ROBDD_int *var, MR_ROBDD_int*word,
                            MR_ROBDD_bitmask *mask);

/* Finds the largest n such that n \not \in set and n \leq *var.  word and */
/* mask must be as set by MR_ROBDD_BITSET_WORD(*var) and MR_ROBDD_BITSET_MASK(*var), */
/* respectively.  The resulting n is placed in *var, and *word and *mask */
/* are updated correspondingly.  Returns MR_TRUE iff there is such an n. */
extern MR_ROBDD_int     MR_ROBDD_prev_nonelement(MR_ROBDD_bitset *set,
                            MR_ROBDD_int *var, MR_ROBDD_int*word,
                            MR_ROBDD_bitmask *mask);

#if !defined(MR_ROBDD_USE_THRESH) && !defined(MR_ROBDD_RESTRICT_SET)

  /* returns a with all variables lo \leq v \leq hi restricted away */
  extern MR_ROBDD_node  *MR_ROBDD_restrictThresh(MR_ROBDD_int lo,
                            MR_ROBDD_int hi, MR_ROBDD_node *a);

  /* returns f \wedge g with all variables lo \leq v \leq hi restricted away */
  extern MR_ROBDD_node  *MR_ROBDD_restricted_glb(MR_ROBDD_int lo,
                            MR_ROBDD_int hi, MR_ROBDD_node *f,
                            MR_ROBDD_node *g);

  /* computes g = f with variable 0 renamed to mapping[0], 1 renamed to */
  /* mapping[1], ... count renamed to mapping[count].  Returns context */
  /* \wedge g with all variables lo \leq v \leq hi restricted away */
  extern MR_ROBDD_node  *MR_ROBDD_abstract_exit(MR_ROBDD_node *context,
                            MR_ROBDD_node *f, MR_ROBDD_int count,
                            MR_ROBDD_int mapping[],
                            MR_ROBDD_int lo, MR_ROBDD_int hi);
  /* returns f \wedge (v0 \leftrightarrow \bigwedge_{i=0}^{n} arr[i]), */
  /* with all variables lo \leq v \leq hi restricted away */
  extern MR_ROBDD_node  *MR_ROBDD_abstract_unify(MR_ROBDD_node *f,
                            MR_ROBDD_int v0, MR_ROBDD_int n,
                            MR_ROBDD_int arr[],
                            MR_ROBDD_int lo, MR_ROBDD_int hi);

#else /* MR_ROBDD_USE_THRESH || MR_ROBDD_RESTRICT_SET */

  /* returns a with all variables lo \leq v \leq hi restricted away */
  extern MR_ROBDD_node  *MR_ROBDD_restrictThresh(MR_ROBDD_int c,
                            MR_ROBDD_node *a);

  /* returns f \wedge g with all variables v \geq c restricted away */
  extern MR_ROBDD_node  *MR_ROBDD_restricted_glb(MR_ROBDD_int c,
                            MR_ROBDD_node *f, MR_ROBDD_node *g);

  /* computes g = f with variable 0 renamed to mapping[0], 1 renamed to */
  /* mapping[1], ... count renamed to mapping[count].  Returns context */
  /* \wedge g with all variables v \geq thresh restricted away */
  extern MR_ROBDD_node  *MR_ROBDD_abstract_exit(MR_ROBDD_node *context,
                            MR_ROBDD_node *f, MR_ROBDD_int count,
                            MR_ROBDD_int mapping[], MR_ROBDD_int thresh);

  /* returns f \wedge (v0 \leftrightarrow \bigwedge_{i=0}^{n} arr[i]), */
  /* with all variables v \eq thresh restricted away */
  extern MR_ROBDD_node  *MR_ROBDD_abstract_unify(MR_ROBDD_node *f,
                            MR_ROBDD_int v0, MR_ROBDD_int n,
                            MR_ROBDD_int arr[], MR_ROBDD_int thresh);

#endif /* !MR_ROBDD_OLD || MR_ROBDD_USE_THRESH */

#if !defined(MR_ROBDD_NEW)
  /* returns the set of all v MR_ROBDD_entailed by f where v \leq MR_ROBDD_topvar */
  extern MR_ROBDD_bitset *MR_ROBDD_vars_entailed(MR_ROBDD_node *f,
                                MR_ROBDD_int MR_ROBDD_topvar);
#else /* MR_ROBDD_NEW */
  /* returns the set of all v MR_ROBDD_entailed by f */
  extern MR_ROBDD_bitset *MR_ROBDD_vars_entailed(MR_ROBDD_node *f);
#endif /* MR_ROBDD_NEW */

/* return the initial set sharing representation for n variables */
extern MR_ROBDD_node    *MR_ROBDD_init_set_sharing(MR_ROBDD_int n);
/* computes the set sharing upward closure of f */
extern MR_ROBDD_node    *MR_ROBDD_upclose(MR_ROBDD_node *f);
/* performs Langen's MR_ROBDD_bin operation, used for set sharing analysis */
extern MR_ROBDD_node    *MR_ROBDD_bin(MR_ROBDD_node *f, MR_ROBDD_node *g);

/* prints out the bryant graph a */
extern void printOut(MR_ROBDD_node *a);

/* for profiling purposes:  return the number of ROBDD nodes in use */
extern MR_ROBDD_int     MR_ROBDD_nodes_in_use(void);
/* for profiling only:  the same as MR_ROBDD_iff_conj_array(), but as efficient */
/* as possible, whatever variables are #defined */
extern MR_ROBDD_node    *MR_ROBDD_testing_iff_conj_array(MR_ROBDD_int v0,
                            MR_ROBDD_int n, MR_ROBDD_int arr[]);

#endif /* MERCURY_BRYANT_H */
