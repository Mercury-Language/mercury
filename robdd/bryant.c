/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 1995, 2001-2004 Peter Schachte and, 2006, 2009 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*****************************************************************
  File     : bryant.c
  Author   : Peter Schachte, based on code by Tania Armstrong
  Purpose  : Manipulation of boolean functions

*****************************************************************/

/*****************************************************************

             Controlling #defined Symbols

  This code is conditionalized on a number of #defined symbols.  They are:

    MR_ROBDD_STATISTICS     Controls collecting of computed and unique
                            table hash performance. If defined,
                            MR_ROBDD_concludeRep() prints out a bunch of
                            statistics.

    MR_ROBDD_CLEAR_CACHES   If defined, all computed and unique tables are
                            cleared on a call to MR_ROBDD_initRep(). This makes
                            running a number of tests in sequence more
                            fair.

    MR_ROBDD_NAIVE          If defined, uses very naive algorithms for
                            MR_ROBDD_iff_conj_array, MR_ROBDD_renameArray,
                            MR_ROBDD_lub, MR_ROBDD_glb, and probably some I've
                            forgotten.

    MR_ROBDD_OLD            If defined, use slightly less naive algorithms for
                            MR_ROBDD_restrictThresh, MR_ROBDD_restricted_glb,
                            MR_ROBDD_vars_entailed, and
                            MR_ROBDD_iff_conj_array.

    MR_ROBDD_USE_THRESH     Like MR_ROBDD_OLD, except that it does use the new
                            code for MR_ROBDD_restrictThresh. Implies
                            MR_ROBDD_OLD.

    MR_ROBDD_USE_RGLB       Like MR_ROBDD_USE_THRESH, except that it also uses
                            the new code for MR_ROBDD_restricted_glb. Implies
                            MR_ROBDD_USE_THRESH.

    MR_ROBDD_NEW            Like MR_ROBDD_USE_RGLB, but uses all the lastest
                            and greatest algorithms. Implies MR_ROBDD_USE_RGLB.

    MR_ROBDD_USE_ITE_CONSTANT
                            Include the MR_ROBDD_ite_constant function, and'
                            use it to speed up the non-MR_ROBDD_NEW version of
                            MR_ROBDD_var_entailed (and MR_ROBDD_vars_entailed).

    MR_ROBDD_SHARING        Include algorithms useful in sharing analysis
                            performed using Boolean functions.

    MR_ROBDD_RESTRICT_SET   Only meaningful if MR_ROBDD_OLD &&
                            !MR_ROBDD_USE_THRESH.  If defined, then we restrict
                            by making a first pass over the ROBDD finding the
                            set of variables present that are greater than the
                            threshold, and then restrict away each variable
                            in that set.

    MR_ROBDD_WHICH          Is defined to "MR_ROBDD_NAIVE", "MR_ROBDD_OLD",
                            "MR_ROBDD_USE_THRESH", "MR_ROBDD_USE_RGLB", or
                            "MR_ROBDD_NEW", depending on which of the
                            MR_ROBDD_NAIVE, MR_ROBDD_OLD, MR_ROBDD_USE_THRESH,
                            and MR_ROBDD_USE_RGLB variables are defined. This
                            is automatically defined by bryant.h based on the
                            above variables.

    MR_ROBDD_VE_GLB         (only when MR_ROBDD_OLD is defined) if defined,
                            MR_ROBDD_var_entailed(fn, v) compares the fn to
                            MR_ROBDD_glb(MR_ROBDD_variableRep(v), fn). The var
                            is entailed if they are equal.  Otherwise computes
                            MR_ROBDD_variableRep(v) -> fn.  The variable is
                            entailed if this is 'MR_ROBDD_one'.

    MR_ROBDD_ELIM_DUPS      If defined, MR_ROBDD_iff_conj_array will not assume
                            that the input array has no duplicates. It will
                            still assume that it's sorted.

    MR_ROBDD_COMPUTED_TABLE Enables the computed table in MR_ROBDD_lub,
                            MR_ROBDD_glb, MR_ROBDD_ite, and
                            MR_ROBDD_restricted_glb.

    MR_ROBDD_EQUAL_TEST     If defined, MR_ROBDD_lub(), MR_ROBDD_glb(), and
                            MR_ROBDD_restricted_glb compare their arguments for
                            equality, and if equal return it as value.
                            MR_ROBDD_ite and MR_ROBDD_ite_var always do this
                            with their last 2 args. The equality test is pretty
                            cheap, and the savings will sometimes be huge, so
                            this should probably always be defined.

    MR_ROBDD_NO_CHEAP_SHIFT If defined, shifting an integer variable number
                            of places is relatively expensive on this platform,
                            and so should be avoided.  In this case we use a
                            table where possible to avoid shifting.

*****************************************************************/

#include "mercury_imp.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <limits.h>
#include "bryant.h"

#ifdef MR_ROBDD_BRYANT_CONSERVATIVE_GC

  /* Don't use pools of nodes with the conservative GC. */
  #undef MR_ROBDD_POOL

  /* Redefine malloc() and free() to use the GC versions */
  #define malloc(n) GC_malloc(n)
  #define free(p) GC_free(p)

#else /* !MR_ROBDD_BRYANT_CONSERVATIVE_GC */

  #define MR_ROBDD_POOL

#endif /* MR_ROBDD_BRYANT_CONSERVATIVE_GC */

#define MR_ROBDD_REVEAL_NODE_POINTER(p) \
    ((MR_ROBDD_node *) MR_ROBDD_REVEAL_POINTER(p))

#define MR_ROBDD_UNUSED_MAPPING -1    /* this MUST BE -1 */

#if !defined(MR_ROBDD_max)
  #define MR_ROBDD_max(a, b) ((a)<(b) ? (b) : (a))
#endif
#if !defined(MR_ROBDD_min)
  #define MR_ROBDD_min(a, b) ((a)<(b) ? (a) : (b))
#endif

#define MR_ROBDD_PERCENTAGE(x, y) ((100.0 * (float)(x)) / (float)(y))

#ifdef MR_ROBDD_POOL
  typedef struct pool {
    MR_ROBDD_node data[MR_ROBDD_POOL_SIZE];
    struct pool *prev;
  } pool;
#endif /* MR_ROBDD_POOL */

#if defined(MR_ROBDD_NO_CHEAP_SHIFT) && MR_ROBDD_BITS_PER_WORD == 32
  MR_ROBDD_bitmask MR_ROBDD_following_bits[MR_ROBDD_BITS_PER_WORD] =
    {
       0xffffffff, 0xfffffffe, 0xfffffffc, 0xfffffff8,
       0xfffffff0, 0xffffffe0, 0xffffffc0, 0xffffff80,
       0xffffff00, 0xfffffe00, 0xfffffc00, 0xfffff800,
       0xfffff000, 0xffffe000, 0xffffc000, 0xffff8000,
       0xffff0000, 0xfffe0000, 0xfffc0000, 0xfff80000,
       0xfff00000, 0xffe00000, 0xffc00000, 0xff800000,
       0xff000000, 0xfe000000, 0xfc000000, 0xf8000000,
       0xf0000000, 0xe0000000, 0xc0000000, 0x80000000
    };

  MR_ROBDD_bitmask MR_ROBDD_preceding_bits[MR_ROBDD_BITS_PER_WORD] =
    {
       0x00000001, 0x00000003, 0x00000007, 0x0000000f,
       0x0000001f, 0x0000003f, 0x0000007f, 0x000000ff,
       0x000001ff, 0x000003ff, 0x000007ff, 0x00000fff,
       0x00001fff, 0x00003fff, 0x00007fff, 0x0000ffff,
       0x0001ffff, 0x0003ffff, 0x0007ffff, 0x000fffff,
       0x001fffff, 0x003fffff, 0x007fffff, 0x00ffffff,
       0x01ffffff, 0x03ffffff, 0x07ffffff, 0x0fffffff,
       0x1fffffff, 0x3fffffff, 0x7fffffff, 0xffffffff
    };
#endif

#if defined(MR_ROBDD_NO_CHEAP_SHIFT) && MR_ROBDD_BITS_PER_WORD == 64
  MR_ROBDD_bitmask MR_ROBDD_following_bits[MR_ROBDD_BITS_PER_WORD] =
    { 
        0xffffffffffffffff, 0xfffffffffffffffe,
        0xfffffffffffffffc, 0xfffffffffffffff8,
        0xfffffffffffffff0, 0xffffffffffffffe0,
        0xffffffffffffffc0, 0xffffffffffffff80,

        0xffffffffffffff00, 0xfffffffffffffe00,
        0xfffffffffffffc00, 0xfffffffffffff800,
        0xfffffffffffff000, 0xffffffffffffe000,
        0xffffffffffffc000, 0xffffffffffff8000,

        0xffffffffffff0000, 0xfffffffffffe0000,
        0xfffffffffffc0000, 0xfffffffffff80000,
        0xfffffffffff00000, 0xffffffffffe00000,
        0xffffffffffc00000, 0xffffffffff800000,

        0xffffffffff000000, 0xfffffffffe000000,
        0xfffffffffc000000, 0xfffffffff8000000,
        0xfffffffff0000000, 0xffffffffe0000000,
        0xffffffffc0000000, 0xffffffff80000000,

        0xffffffff00000000, 0xfffffffe00000000,
        0xfffffffc00000000, 0xfffffff800000000,
        0xfffffff000000000, 0xffffffe000000000,
        0xffffffc000000000, 0xffffff8000000000,

        0xffffff0000000000, 0xfffffe0000000000,
        0xfffffc0000000000, 0xfffff80000000000,
        0xfffff00000000000, 0xffffe00000000000,
        0xffffc00000000000, 0xffff800000000000,

        0xffff000000000000, 0xfffe000000000000,
        0xfffc000000000000, 0xfff8000000000000,
        0xfff0000000000000, 0xffe0000000000000,
        0xffc0000000000000, 0xff80000000000000,

        0xff00000000000000, 0xfe00000000000000,
        0xfc00000000000000, 0xf800000000000000,
        0xf000000000000000, 0xe000000000000000,
        0xc000000000000000, 0x8000000000000000
    };

  MR_ROBDD_bitmask MR_ROBDD_preceding_bits[MR_ROBDD_BITS_PER_WORD] =
    {
       0x0000000000000001, 0x0000000000000003,
       0x0000000000000007, 0x000000000000000f,
       0x000000000000001f, 0x000000000000003f,
       0x000000000000007f, 0x00000000000000ff,

       0x00000000000001ff, 0x00000000000003ff,
       0x00000000000007ff, 0x0000000000000fff,
       0x0000000000001fff, 0x0000000000003fff,
       0x0000000000007fff, 0x000000000000ffff,

       0x000000000001ffff, 0x000000000003ffff,
       0x000000000007ffff, 0x00000000000fffff,
       0x00000000001fffff, 0x00000000003fffff,
       0x00000000007fffff, 0x0000000000ffffff,

       0x0000000001ffffff, 0x0000000003ffffff,
       0x0000000007ffffff, 0x000000000fffffff,
       0x000000001fffffff, 0x000000003fffffff,
       0x000000007fffffff, 0x00000000ffffffff,

       0x00000001ffffffff, 0x00000003ffffffff,
       0x00000007ffffffff, 0x0000000fffffffff,
       0x0000001fffffffff, 0x0000003fffffffff,
       0x0000007fffffffff, 0x000000ffffffffff,

       0x000001ffffffffff, 0x000003ffffffffff,
       0x000007ffffffffff, 0x00000fffffffffff,
       0x00001fffffffffff, 0x00003fffffffffff,
       0x00007fffffffffff, 0x0000ffffffffffff,

       0x0001ffffffffffff, 0x0003ffffffffffff,
       0x0007ffffffffffff, 0x000fffffffffffff,
       0x001fffffffffffff, 0x003fffffffffffff,
       0x007fffffffffffff, 0x00ffffffffffffff,

       0x01ffffffffffffff, 0x03ffffffffffffff,
       0x07ffffffffffffff, 0x0fffffffffffffff,
       0x1fffffffffffffff, 0x3fffffffffffffff,
       0x7fffffffffffffff, 0xffffffffffffffff
    };
#endif

unsigned char MR_ROBDD_first_one_bit[256] =
    {255, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /*  16 */
       4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /*  32 */
       5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /*  48 */
       4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /*  64 */
       6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /*  80 */
       4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /*  96 */
       5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 112 */
       4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 128 */
       7, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 144 */
       4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 160 */
       5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 176 */
       4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 192 */
       6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 208 */
       4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 224 */
       5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 240 */
       4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, /* 256 */
    };

unsigned char MR_ROBDD_last_one_bit[256] =
    {255, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, /*  16 */
       4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, /*  32 */
       5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, /*  48 */
       5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, /*  64 */
       6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, /*  80 */
       6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, /*  96 */
       6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, /* 112 */
       6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, /* 128 */
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, /* 144 */
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, /* 160 */
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, /* 176 */
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, /* 192 */
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, /* 208 */
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, /* 224 */
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, /* 240 */
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, /* 256 */
    };

/* automatically initialized to all 0 bits: */
MR_ROBDD_bitset MR_ROBDD_emptyset;

/****************************************************************

               Macros to avoid #ifdefs

 ****************************************************************/

#if defined(MR_ROBDD_COMPUTED_TABLE) && defined(MR_ROBDD_CLEAR_CACHES)
  #define MR_ROBDD_CLEAR_CACHE(op)    \
    MR_memset(op##_computed_cache, 0, sizeof(op##_computed_cache))
#else /* !MR_ROBDD_CLEAR_CACHES || !MR_ROBDD_COMPUTED_TABLE */
  #define MR_ROBDD_CLEAR_CACHE(op)
#endif

#if defined(MR_ROBDD_STATISTICS)

  /* The largest bucket size to be separately counted.  Larger buckets
   * will be listed as "> MR_ROBDD_MAX_COUNT." 
   */
  #define MR_ROBDD_MAX_COUNT            1000

  MR_ROBDD_int MR_ROBDD_unique_table_hits, MR_ROBDD_unique_table_misses;

  #define MR_ROBDD_DECLARE_FN_COUNT(op) MR_ROBDD_int op##_count;

  #define MR_ROBDD_COUNT_FN(fn)         (++fn##_count)

  #define MR_ROBDD_INIT_FN_COUNT(fn)    fn##_count = 0
  #define MR_ROBDD_PRINT_FN_COUNT(fn) \
    if (fn##_count!=0) printf("%6ld calls to " #fn "\n", (long) fn##_count);

  #define MR_ROBDD_COUNT_UNIQUE_HIT     (++MR_ROBDD_unique_table_hits)
  #define MR_ROBDD_COUNT_UNIQUE_MISS    (++MR_ROBDD_unique_table_misses)

  #if defined(MR_ROBDD_COMPUTED_TABLE)

    #define MR_ROBDD_COUNT_HIT(op)  (++op##_computed_hits)
    #define MR_ROBDD_COUNT_MISS(op) (++op##_computed_misses, ++cache->count)
    #define MR_ROBDD_INIT_CACHE(op)                                         \
    do {                                                                    \
        op##_computed_misses = 0;                                           \
        op##_computed_hits = 0;                                             \
        MR_ROBDD_CLEAR_CACHE(op);                                           \
    } while (0)

    #define MR_ROBDD_CACHE_COUNT_MEMBER MR_ROBDD_int count;
    #define MR_ROBDD_PRINT_CACHE_PERFORMANCE(op)                            \
    do {                                                                    \
        if (op##_computed_misses > 0 ) {                                    \
            MR_ROBDD_int i, size_count[MR_ROBDD_MAX_COUNT+2];               \
            printf(                                                         \
                #op " computed table:  %ld hits, %ld misses, %.2f%% hit rate\n",\
                (long) op##_computed_hits, (long) op##_computed_misses,     \
            MR_ROBDD_PERCENTAGE(op##_computed_hits,                         \
               op##_computed_hits + op##_computed_misses));                 \
            MR_memset(size_count, 0, sizeof(size_count));                   \
            for (i=0; i<MR_ROBDD_COMPUTED_TABLE_SIZE; ++i) {                \
                MR_ROBDD_int count = op##_computed_cache[i].count;          \
                ++size_count[(count<=MR_ROBDD_MAX_COUNT ? count :           \
                    MR_ROBDD_MAX_COUNT+1)];                                 \
            }                                                               \
            MR_ROBDD_print_distribution(size_count, MR_ROBDD_MAX_COUNT);    \
        }                                                                   \
    } while (0)

  #else /* !MR_ROBDD_COMPUTED_TABLE */

    #define MR_ROBDD_PRINT_CACHE_PERFORMANCE(op)

  #endif /* MR_ROBDD_COMPUTED_TABLE */
#else /* ! MR_ROBDD_STATISTICS */

  #define MR_ROBDD_DECLARE_FN_COUNT(op)
  #define MR_ROBDD_COUNT_FN(fn)
  #define MR_ROBDD_INIT_FN_COUNT(fn)
  #define MR_ROBDD_PRINT_FN_COUNT(fn)
  #define MR_ROBDD_COUNT_UNIQUE_HIT
  #define MR_ROBDD_COUNT_UNIQUE_MISS
  #define MR_ROBDD_INIT_CACHE(op) MR_ROBDD_CLEAR_CACHE(op)
  #define MR_ROBDD_PRINT_CACHE_PERFORMANCE(op)

  #if defined(MR_ROBDD_COMPUTED_TABLE)
    #define MR_ROBDD_COUNT_HIT(fn)
    #define MR_ROBDD_COUNT_MISS(fn)
    #define MR_ROBDD_CACHE_COUNT_MEMBER
  #endif /* MR_ROBDD_COMPUTED_TABLE */

#endif /* MR_ROBDD_STATISTICS */

#if defined(MR_ROBDD_EQUAL_TEST)
  #define MR_ROBDD_DIRECT_EQUAL_TEST(f, g, result)   \
    if ((f) == (g)) return (result)
  #define MR_ROBDD_ELSE_TRY_EQUAL_TEST(f, g, result) \
    else MR_ROBDD_DIRECT_EQUAL_TEST(f, g, result);
#else /* ! MR_ROBDD_EQUAL_TEST */
  #define MR_ROBDD_DIRECT_EQUAL_TEST(f, g, result)
  #define MR_ROBDD_ELSE_TRY_EQUAL_TEST(f, g, result)
#endif

#if defined(MR_ROBDD_CLEAR_CACHES)
  #if defined(MR_ROBDD_POOL)
    #define MR_ROBDD_INIT_UNIQUE_TABLE                                      \
        do {                                                                \
            MR_memset(MR_ROBDD_unique_table, (char) 0,                      \
                sizeof(MR_ROBDD_unique_table));                             \
            while (MR_ROBDD_curr_pool!=NULL) {                              \
                pool *p = MR_ROBDD_curr_pool->prev;                         \
                free(MR_ROBDD_curr_pool);                                   \
                MR_ROBDD_curr_pool = p;                                     \
            }                                                               \
            MR_ROBDD_curr_pool_ptr = MR_ROBDD_curr_pool_end_ptr = NULL;     \
            MR_ROBDD_node_count = 0;                                        \
        } while (0)

  #else /* !MR_ROBDD_POOL */
    #define MR_ROBDD_INIT_UNIQUE_TABLE                                      \
    MR_memset(MR_ROBDD_unique_table, (char) 0, sizeof(MR_ROBDD_unique_table));
   #endif /* MR_ROBDD_POOL */
#else /* !MR_ROBDD_CLEAR_CACHES */
  #define MR_ROBDD_INIT_UNIQUE_TABLE
#endif /* MR_ROBDD_CLEAR_CACHES */

/********************************************************************

              Caching of computed values

  For improved efficiency, if MR_ROBDD_COMPUTED_TABLE is defined we maintain a
  cache of previously computed values to certain functions, and use
  this to avoid costly computations when possible.  This is
  particularly important for ROBDDs, because the high degree of fan-in
  causes frequent repeated computations.

  Because caching is in many ways extraneous to the functions being
  cached, we put the caching code here as macros, so that the
  presentation of the algorithms is minimally impacted by caching.

 ********************************************************************/

#if defined(MR_ROBDD_COMPUTED_TABLE)

  #if defined(MR_ROBDD_STATISTICS)
    #define MR_ROBDD_DECLARE_CACHE(op, MR_ROBDD_type)                   \
    static MR_ROBDD_int op##_computed_hits;                             \
    static MR_ROBDD_int op##_computed_misses;                           \
    static MR_ROBDD_type op##_computed_cache[MR_ROBDD_COMPUTED_TABLE_SIZE]
  #else /* !MR_ROBDD_STATISTICS */
    #define MR_ROBDD_DECLARE_CACHE(op, MR_ROBDD_type)                   \
    static MR_ROBDD_type op##_computed_cache[MR_ROBDD_COMPUTED_TABLE_SIZE]
  #endif /* MR_ROBDD_STATISTICS */

/********************* the cache for MR_ROBDD_ite **************************/

  #define MR_ROBDD_TERNARY_NODE_HASH(f, g, h) \
    (((MR_ROBDD_INTCAST(f)>>4)+MR_ROBDD_INTCAST(g)+(MR_ROBDD_INTCAST(h)<<1)) \
        % MR_ROBDD_COMPUTED_TABLE_SIZE)

  typedef struct {
    MR_ROBDD_node *f;
    MR_ROBDD_node *g;
    MR_ROBDD_node *h;
    MR_ROBDD_node *result;
    MR_ROBDD_CACHE_COUNT_MEMBER
  } ite_cache_entry;

  MR_ROBDD_DECLARE_CACHE(MR_ROBDD_ite, ite_cache_entry);
  #if defined(MR_ROBDD_USE_ITE_CONSTANT)
    MR_ROBDD_DECLARE_CACHE(MR_ROBDD_ite_constant, ite_cache_entry);
  #endif /* MR_ROBDD_USE_ITE_CONSTANT */

  #define MR_ROBDD_DECLARE_ITE_CACHE_ENTRY ite_cache_entry *cache;

  #define MR_ROBDD_TRY_ITE_CACHE(n1, n2, n3, op)                            \
    do {                                                                    \
        cache = &op##_computed_cache[MR_ROBDD_TERNARY_NODE_HASH(n1,n2,n3)]; \
        if (cache->f==n1 && cache->g==n2 && cache->h==n3) {                 \
            MR_ROBDD_COUNT_HIT(op);                                         \
            return cache->result;                                           \
        }                                                                   \
    } while (0)

  #define MR_ROBDD_UPDATE_ITE_CACHE(n1, n2, n3, MR_ROBDD_node, op)          \
    do {                                                                    \
        cache->f = n1;                                                      \
        cache->g = n2;                                                      \
        cache->h = n3;                                                      \
        cache->result = MR_ROBDD_node;                                      \
        MR_ROBDD_COUNT_MISS(op);                                            \
    } while (0)

/******************** the cache for unary operations ************/

  #define MR_ROBDD_UNARY_NODE_HASH(a) \
      (MR_ROBDD_INTCAST(a) % MR_ROBDD_COMPUTED_TABLE_SIZE)

  typedef struct {
    MR_ROBDD_node *f;
    MR_ROBDD_node *result;
    MR_ROBDD_CACHE_COUNT_MEMBER
  } unary_cache_entry;

  #if defined(MR_ROBDD_SHARING)
    MR_ROBDD_DECLARE_CACHE(MR_ROBDD_upclose, unary_cache_entry);
    #if defined(MR_ROBDD_NEW)
      MR_ROBDD_DECLARE_CACHE(MR_ROBDD_complete_one, unary_cache_entry);
    #endif /* MR_ROBDD_NEW */
  #endif /* MR_ROBDD_SHARING */

  #define MR_ROBDD_DECLARE_UNARY_CACHE_ENTRY unary_cache_entry *cache;

  #define MR_ROBDD_UPDATE_UNARY_CACHE(n, MR_ROBDD_node, op)                 \
    do {                                                                    \
        cache->f = n;                                                       \
        cache->result = MR_ROBDD_node;                                      \
        MR_ROBDD_COUNT_MISS(op);                                            \
    } while (0)

  #define MR_ROBDD_TRY_UNARY_CACHE(n, op)                                   \
    do {                                                                    \
        cache = &((op##_computed_cache)[MR_ROBDD_UNARY_NODE_HASH(n)]);      \
        if (cache->f==(n)) {                                                \
            MR_ROBDD_COUNT_HIT(op);                                         \
            return cache->result;                                           \
        }                                                                   \
    } while (0)

/******************** the cache for MR_ROBDD_var_entailed ************/

  #define MR_ROBDD_VAR_ENTAILED_HASH(a, n) \
      ((MR_ROBDD_INTCAST(a)+n) % MR_ROBDD_COMPUTED_TABLE_SIZE)

  typedef struct {
    MR_ROBDD_node *f;
    MR_ROBDD_int  n;
    MR_ROBDD_int  result;
    MR_ROBDD_CACHE_COUNT_MEMBER
  } var_entailed_cache_entry;

  #if defined(MR_ROBDD_USE_RGLB)
    MR_ROBDD_DECLARE_CACHE(MR_ROBDD_var_entailed, var_entailed_cache_entry);
  #endif /* MR_ROBDD_USE_RGLB */

  #define MR_ROBDD_DECLARE_VAR_ENTAILED_CACHE_ENTRY \
    var_entailed_cache_entry *cache;

  #define MR_ROBDD_UPDATE_VAR_ENTAILED_CACHE(MR_ROBDD_node, var, val)       \
    do {                                                                    \
        cache->f = MR_ROBDD_node;                                           \
        cache->n = var;                                                     \
        cache->result = val;                                                \
        MR_ROBDD_COUNT_MISS(MR_ROBDD_var_entailed);                         \
    } while (0)

  #define MR_ROBDD_TRY_VAR_ENTAILED_CACHE(MR_ROBDD_node, var)               \
    do {                                                                    \
        cache = &((MR_ROBDD_var_entailed_computed_cache)                    \
              [MR_ROBDD_VAR_ENTAILED_HASH(MR_ROBDD_node, var)]);            \
        if (cache->f==(MR_ROBDD_node) && cache->n==(var)) {                 \
            MR_ROBDD_COUNT_HIT(op);                                         \
            return cache->result;                                           \
        }                                                                   \
    } while (0)

/**************** the cache for unary set-valued operations ********/

  typedef struct {
    MR_ROBDD_node *f;
    MR_ROBDD_bitset result;
    MR_ROBDD_CACHE_COUNT_MEMBER
  } unary_bitset_cache_entry;

  #if defined(MR_ROBDD_NEW)
    MR_ROBDD_DECLARE_CACHE(MR_ROBDD_vars_entailed, unary_bitset_cache_entry);
  #endif

  #define MR_ROBDD_DECLARE_UNARY_BITSET_CACHE_ENTRY                         \
    unary_bitset_cache_entry *cache;

  #define MR_ROBDD_UPDATE_UNARY_BITSET_CACHE(n, set, op)                    \
    do {                                                                    \
        cache->f = n;                                                       \
        cache->result = set;                                                \
        MR_ROBDD_COUNT_MISS(op);                                            \
    } while (0)

  #define MR_ROBDD_TRY_UNARY_BITSET_CACHE(n, op)                            \
    do {                                                                    \
        cache = &((op##_computed_cache)[MR_ROBDD_UNARY_NODE_HASH(n)]);      \
        if (cache->f==(n)) {                                                \
            MR_ROBDD_COUNT_HIT(op);                                         \
            return &cache->result;                                          \
        }                                                                   \
    } while (0)

/******************** the cache for symmetric binary operations ************/

/*
** NB:  Since MR_ROBDD_glb and MR_ROBDD_lub are commutative, cache entries
** will work both ways round, so we want a symmetrical cache, ie, (a, b) should
** hash to the same value as (b, a).  We achieve this by first exchanging
** a and b if a > b (comparing their addresses) in MR_ROBDD_TRY_BIN_CACHE.
** We assume that they won't be changed (or exchanged) before
** MR_ROBDD_UPDATE_BIN_CACHE is called.
*/

  #define MR_ROBDD_BINARY_NODE_HASH(a, b) \
      ((MR_ROBDD_INTCAST(a)+(MR_ROBDD_INTCAST(b)<<1)) % MR_ROBDD_COMPUTED_TABLE_SIZE)

  typedef struct {
    MR_ROBDD_node *f;
    MR_ROBDD_node *g;
    MR_ROBDD_node *result;
    MR_ROBDD_CACHE_COUNT_MEMBER
  } bin_cache_entry;

  MR_ROBDD_DECLARE_CACHE(MR_ROBDD_glb, bin_cache_entry);
  MR_ROBDD_DECLARE_CACHE(MR_ROBDD_lub, bin_cache_entry);
  #if defined(MR_ROBDD_SHARING)
    MR_ROBDD_DECLARE_CACHE(MR_ROBDD_bin, bin_cache_entry);
    MR_ROBDD_DECLARE_CACHE(MR_ROBDD_complete, bin_cache_entry);
  #endif

  #define MR_ROBDD_DECLARE_BIN_CACHE_ENTRY bin_cache_entry *cache;

  #define MR_ROBDD_UPDATE_BIN_CACHE(n1, n2, MR_ROBDD_node, op)              \
    do {                                                                    \
        cache->f = n1;                                                      \
        cache->g = n2;                                                      \
        cache->result = MR_ROBDD_node;                                      \
        MR_ROBDD_COUNT_MISS(op);                                            \
    } while (0)

  #define MR_ROBDD_TRY_BIN_CACHE(n1, n2, op)                                \
    do {                                                                    \
        if (n2 < n1) {                                                      \
            MR_ROBDD_node *temp = (n2);                                     \
            (n2) = (n1);                                                    \
            (n1) = temp;                                                    \
        }                                                                   \
        cache = &((op##_computed_cache)[MR_ROBDD_BINARY_NODE_HASH(n1,n2)]); \
        if (cache->f==(n1) && cache->g==(n2)) {                             \
            MR_ROBDD_COUNT_HIT(op);                                         \
            return cache->result;                                           \
        }                                                                   \
    } while (0)

/******************** the cache for asymmetric binary operations ************/

  #if defined(MR_ROBDD_SHARING) && defined(MR_ROBDD_NEW)
    MR_ROBDD_DECLARE_CACHE(MR_ROBDD_complete_one_or, bin_cache_entry);
  #endif /* MR_ROBDD_SHARING && MR_ROBDD_NEW */

  #if defined(MR_ROBDD_USE_THRESH)
    MR_ROBDD_DECLARE_CACHE(MR_ROBDD_restrictThresh, bin_cache_entry);
  #endif /* MR_ROBDD_USE_THRESH */

  #if (defined(MR_ROBDD_SHARING) && defined(MR_ROBDD_NEW)) || defined(MR_ROBDD_USE_THRESH)

    #define MR_ROBDD_DECLARE_ASYM_BIN_CACHE_ENTRY bin_cache_entry *cache;

    #define MR_ROBDD_UPDATE_ASYM_BIN_CACHE(n1, n2, MR_ROBDD_node, op)       \
        MR_ROBDD_UPDATE_BIN_CACHE(n1, n2, MR_ROBDD_node, op)

  #define MR_ROBDD_TRY_ASYM_BIN_CACHE(n1, n2, op)                           \
    do {                                                                    \
        cache = &((op##_computed_cache)[MR_ROBDD_BINARY_NODE_HASH(n1,n2)]); \
        if (cache->f==(n1) && cache->g==(n2)) {                             \
            MR_ROBDD_COUNT_HIT(op);                                         \
            return cache->result;                                           \
        }                                                                   \
    } while (0)

  #endif /* (MR_ROBDD_SHARING && MR_ROBDD_NEW) || MR_ROBDD_USE_THRESH */

/**************************** the cache for rglb ***********************/

  #if defined(MR_ROBDD_USE_RGLB)

    typedef struct {
        MR_ROBDD_node   *f;
        MR_ROBDD_node   *g;
        MR_ROBDD_node   *result;
        MR_ROBDD_int             thresh;
        MR_ROBDD_CACHE_COUNT_MEMBER
    } rglb_cache_entry;

    MR_ROBDD_DECLARE_CACHE(rglb, rglb_cache_entry);

    #define MR_ROBDD_DECLARE_RGLB_CACHE_ENTRY rglb_cache_entry *cache;

    #define MR_ROBDD_TRY_RGLB_CACHE(n1, n2, th)                             \
    do {                                                                    \
        if (n2 < n1) {                                                      \
            MR_ROBDD_node *temp = (n2);                                     \
            (n2) = (n1);                                                    \
            (n1) = temp;                                                    \
        }                                                                   \
        cache = &rglb_computed_cache[MR_ROBDD_BINARY_NODE_HASH(n1,n2)];     \
        if (cache->f==(n1) && cache->g==(n2) && cache->thresh >= th) {      \
            MR_ROBDD_COUNT_HIT(rglb);                                       \
            if (cache->thresh == th) {                                      \
                return cache->result;                                       \
            }                                                               \
            return MR_ROBDD_restrictThresh(th, cache->result);              \
        }                                                                   \
    } while (0)

    #define MR_ROBDD_UPDATE_RGLB_CACHE(n1, n2, th,MR_ROBDD_node)            \
    do {                                                                    \
        cache->f = n1;                                                      \
        cache->g = n2;                                                      \
        cache->thresh = th;                                                 \
        cache->result = MR_ROBDD_node;                                      \
        MR_ROBDD_COUNT_MISS(rglb);                                          \
    } while (0)

  #endif /* MR_ROBDD_USE_RGLB */

/************************ the cache for MR_ROBDD_complete_or *******************/

  #if defined(MR_ROBDD_SHARING) && defined(MR_ROBDD_NEW)
    typedef struct {
        MR_ROBDD_node *f;
        MR_ROBDD_node *g;
        MR_ROBDD_node *prev;
        MR_ROBDD_node *result;
        MR_ROBDD_CACHE_COUNT_MEMBER
    } complete_or_cache_entry;

    MR_ROBDD_DECLARE_CACHE(MR_ROBDD_complete_or, complete_or_cache_entry);

    #define MR_ROBDD_DECLARE_COMPLETE_OR_CACHE_ENTRY \
        complete_or_cache_entry *cache;

    #define MR_ROBDD_TRY_COMPLETE_OR_CACHE(n1, n2, pr)                      \
    do {                                                                    \
        if (n2 < n1) {                                                      \
            MR_ROBDD_node *temp = (n2);                                     \
            (n2) = (n1);                                                    \
            (n1) = temp;                                                    \
        }                                                                   \
        cache = &complete_or_computed_cache                                 \
            [MR_ROBDD_TERNARY_NODE_HASH(n1,n2,pr)];                         \
        if ((cache->f==n1 && cache->g==n2 && cache->prev==pr)) {            \
            MR_ROBDD_COUNT_HIT(MR_ROBDD_complete_or);                       \
            return cache->result;                                           \
        }                                                                   \
    } while (0)

    #define MR_ROBDD_UPDATE_COMPLETE_OR_CACHE(n1, n2, pr, MR_ROBDD_node)    \
    do {                                                                    \
        cache->f = n1;                                                      \
        cache->g = n2;                                                      \
        cache->prev = pr;                                                   \
        cache->result = MR_ROBDD_node;                                      \
        MR_ROBDD_COUNT_MISS(MR_ROBDD_complete_or);                          \
    } while (0)

  #endif /* MR_ROBDD_SHARING && MR_ROBDD_NEW */

/********************* the cache for MR_ROBDD_ite_var ***********************/

  #if defined(MR_ROBDD_NEW)

    #define MR_ROBDD_ITE_VAR_COMPUTED_HASH(f, g, h) \
      ((f+MR_ROBDD_INTCAST(g)+(MR_ROBDD_INTCAST(h)<<1)) \
        % MR_ROBDD_COMPUTED_TABLE_SIZE)

    typedef struct {
        MR_ROBDD_int    f;
        MR_ROBDD_node   *g;
        MR_ROBDD_node   *h;
        MR_ROBDD_node   *result;
        MR_ROBDD_CACHE_COUNT_MEMBER
    } ite_var_cache_entry;

    MR_ROBDD_DECLARE_CACHE(MR_ROBDD_ite_var, ite_var_cache_entry);

    #define MR_ROBDD_DECLARE_ITE_VAR_CACHE_ENTRY ite_var_cache_entry *cache;

    #define MR_ROBDD_TRY_ITE_VAR_CACHE(n1, n2, h)                           \
    do {                                                                    \
        cache = &MR_ROBDD_ite_var_computed_cache[                           \
            MR_ROBDD_ITE_VAR_COMPUTED_HASH(n1,n2,h)];                       \
        if (cache->f==n1 && cache->g==n2 && cache->h==h) {                  \
            MR_ROBDD_COUNT_HIT(MR_ROBDD_ite_var);                           \
            return cache->result;                                           \
        }                                                                   \
    } while (0)

    #define MR_ROBDD_UPDATE_ITE_VAR_CACHE(n1, n2, h, MR_ROBDD_node)         \
    do {                                                                    \
        cache->f = n1;                                                      \
        cache->g = n2;                                                      \
        cache->h = h;                                                       \
        cache->result = MR_ROBDD_node;                                      \
        MR_ROBDD_COUNT_MISS(MR_ROBDD_ite_var);                              \
    } while (0)

  #endif /* MR_ROBDD_NEW */

#else /* !MR_ROBDD_COMPUTED_TABLE */

/**************************** no caching at all **************************/

  #define MR_ROBDD_DECLARE_ITE_CACHE_ENTRY
  #define MR_ROBDD_TRY_ITE_CACHE(n1, n2, n3, op)
  #define MR_ROBDD_UPDATE_ITE_CACHE(n1, n2, n3, MR_ROBDD_node, op)

  #define MR_ROBDD_DECLARE_UNARY_CACHE_ENTRY
  #define MR_ROBDD_UPDATE_UNARY_CACHE(n, MR_ROBDD_node, op)
  #define MR_ROBDD_TRY_UNARY_CACHE(n, op)

  #define MR_ROBDD_DECLARE_VAR_ENTAILED_CACHE_ENTRY
  #define MR_ROBDD_UPDATE_VAR_ENTAILED_CACHE(MR_ROBDD_node, var, val)
  #define MR_ROBDD_TRY_VAR_ENTAILED_CACHE(MR_ROBDD_node, var)

  #define MR_ROBDD_DECLARE_UNARY_BITSET_CACHE_ENTRY
  #define MR_ROBDD_UPDATE_UNARY_BITSET_CACHE(n, set, op)
  #define MR_ROBDD_TRY_UNARY_BITSET_CACHE(n, op)

  #define MR_ROBDD_DECLARE_BIN_CACHE_ENTRY
  #define MR_ROBDD_TRY_BIN_CACHE(n1, n2, op)
  #define MR_ROBDD_UPDATE_BIN_CACHE(n1, n2, MR_ROBDD_node, op)

  #define MR_ROBDD_DECLARE_ASYM_BIN_CACHE_ENTRY
  #define MR_ROBDD_UPDATE_ASYM_BIN_CACHE(n1, n2, MR_ROBDD_node, op)
  #define MR_ROBDD_TRY_ASYM_BIN_CACHE(n1, n2, op)

  #define MR_ROBDD_DECLARE_RGLB_CACHE_ENTRY
  #define MR_ROBDD_TRY_RGLB_CACHE(n1, n2, th)
  #define MR_ROBDD_UPDATE_RGLB_CACHE(n1, n2, th, MR_ROBDD_node)

  #define MR_ROBDD_DECLARE_COMPLETE_OR_CACHE_ENTRY
  #define MR_ROBDD_TRY_COMPLETE_OR_CACHE(n1, n2, pr)
  #define MR_ROBDD_UPDATE_COMPLETE_OR_CACHE(n1, n2, pr, MR_ROBDD_node)

  #define MR_ROBDD_DECLARE_ITE_VAR_CACHE_ENTRY
  #define MR_ROBDD_TRY_ITE_VAR_CACHE(n1, n2, h)
  #define MR_ROBDD_UPDATE_ITE_VAR_CACHE(n1, n2, h, MR_ROBDD_node)

  #undef MR_ROBDD_COMPUTED_TABLE_SIZE
  #define MR_ROBDD_COMPUTED_TABLE_SIZE 0

#endif /* MR_ROBDD_COMPUTED_TABLE */

/****************************************************************

               The Unique Table

 ****************************************************************/

static MR_ROBDD_BRYANT_hidden_node_pointer
    MR_ROBDD_unique_table[MR_ROBDD_UNIQUE_TABLE_SIZE];

#define MR_ROBDD_UNIQUE_HASH(var, tr, fa) \
  (((var)+MR_ROBDD_INTCAST(tr)+(MR_ROBDD_INTCAST(fa)<<1)) \
    % MR_ROBDD_UNIQUE_TABLE_SIZE)

/****************************************************************

                 Prototypes

 ****************************************************************/

extern void MR_ROBDD_printBryant(MR_ROBDD_node *a);

/* if then else algorithm */
MR_ROBDD_node   *MR_ROBDD_ite(MR_ROBDD_node *f, MR_ROBDD_node *g,
                    MR_ROBDD_node *h);
MR_ROBDD_node   *MR_ROBDD_restricted_iff_conj_array(MR_ROBDD_int v0,
                    MR_ROBDD_int n, MR_ROBDD_int arr[], MR_ROBDD_int thresh);

MR_ROBDD_node   *MR_ROBDD_renameArray(MR_ROBDD_node *in, MR_ROBDD_int count,
                    MR_ROBDD_int mappping[]);
MR_ROBDD_node   *MR_ROBDD_reverseRenameArray(MR_ROBDD_node *in,
                    MR_ROBDD_int count, MR_ROBDD_int rev_mappping[]);

MR_ROBDD_node   *MR_ROBDD_complete(MR_ROBDD_node *f, MR_ROBDD_node *g);
MR_ROBDD_node   *MR_ROBDD_bin_univ(MR_ROBDD_node *f);

static int      MR_ROBDD_intcompare(const void *i, const void *j);

/****************************************************************

             Inline Bit Set Stuff

 ****************************************************************/

/*
** MR_ROBDD_next_element()
**     finds the next element in set beginning with var,
**    and updates var, word, and mask to refer to it.
** MR_ROBDD_prev_element()
**     finds the next earlier element in set.
** MR_ROBDD_next_nonelement()
**     finds the next potential element of set that is in fact not an element
**     of set.
** MR_ROBDD_prev_nonelement()
**     finds the next earlier non-element of set.
**
** NB: if the initally supplied element is a member of the set,
** MR_ROBDD_next_element and MR_ROBDD_prev_element will happily return
** that MR_ROBDD_one. Similarly, MR_ROBDD_next_nonelement and
** MR_ROBDD_prev_nonelement will happily return the initial input if it
** fits the bill. That is, these find the next or next earlier element
** INLCUDING THE INITIAL ONE that meets the criterion.
*/

#if defined(MR_ROBDD_NO_CHEAP_SHIFT)

__inline MR_ROBDD_int
MR_ROBDD_next_element(MR_ROBDD_bitset *set, MR_ROBDD_int *var,
    MR_ROBDD_int *word, MR_ROBDD_bitmask *mask)
{
    MR_ROBDD_int vr = *var;
    MR_ROBDD_int wd = *word;
    MR_ROBDD_bitmask f = MR_ROBDD_FOLLOWING_BITS(vr&(MR_ROBDD_BITS_PER_WORD-1));
    MR_ROBDD_bitmask *ptr = &(set->bits[wd]);
    MR_ROBDD_bitmask bits = *ptr;
    MR_ROBDD_bitmask msk = *mask;

    assert(vr >= 0 && vr < MR_ROBDD_MAXVAR);

    if ((bits&f) == 0) {
        do {
            if (++wd > ((MR_ROBDD_MAXVAR-1)/MR_ROBDD_BITS_PER_WORD)) {
                return MR_FALSE;
            }
        } while ((bits=*++ptr) == 0);
        vr = wd<<MR_ROBDD_LOG_BITS_PER_WORD;
        msk = 1;
    }
    /* I know there's a later bit set in bits, so this is safe */
    while ((bits&msk) == 0) {
        ++vr;
        msk <<= 1;
        assert(vr < (wd+1)<<MR_ROBDD_LOG_BITS_PER_WORD);
    }
    *var = vr;
    *word = wd;
    *mask = msk;
    return MR_TRUE;
}

#else /* !MR_ROBDD_NO_CHEAP_SHIFT */

__inline MR_ROBDD_int
MR_ROBDD_next_element(MR_ROBDD_bitset *set, MR_ROBDD_int *var,
    MR_ROBDD_int *word, MR_ROBDD_bitmask *mask)
{
    MR_ROBDD_int vr = *var;
    MR_ROBDD_int wd = *word;
    MR_ROBDD_bitmask *ptr = &(set->bits[wd]);
    MR_ROBDD_bitmask bits =
        *ptr&MR_ROBDD_FOLLOWING_BITS(vr&(MR_ROBDD_BITS_PER_WORD-1));

    assert(vr >= 0 && vr < MR_ROBDD_MAXVAR);

    while (bits == 0) {
        if (++wd > (MR_ROBDD_MAXVAR-1)/MR_ROBDD_BITS_PER_WORD) {
            return MR_FALSE;
        }
        bits = *++ptr;
    }
    vr = wd<<MR_ROBDD_LOG_BITS_PER_WORD;
    /* I know there's a later bit set in bits, so this is safe */
    while ((bits & MR_ROBDD_CHAR_MASK) == 0) {
        bits >>= MR_ROBDD_BITS_PER_CHAR;
        vr += MR_ROBDD_BITS_PER_CHAR;
        assert(vr < (wd+1)<<MR_ROBDD_LOG_BITS_PER_WORD);
    }
    vr += MR_ROBDD_first_one_bit[bits & MR_ROBDD_CHAR_MASK];

    *var = vr;
    *word = wd;
    *mask = 1<<(vr&(MR_ROBDD_BITS_PER_WORD-1));
    return MR_TRUE;
}

#endif /* MR_ROBDD_NO_CHEAP_SHIFT */

#if defined(MR_ROBDD_NO_CHEAP_SHIFT)

__inline MR_ROBDD_int
MR_ROBDD_prev_element(MR_ROBDD_bitset *set, MR_ROBDD_int *var,
    MR_ROBDD_int *word, MR_ROBDD_bitmask *mask)
{
    MR_ROBDD_int vr = *var;
    MR_ROBDD_int wd = *word;

    assert(vr >= 0 && vr < MR_ROBDD_MAXVAR);

    MR_ROBDD_bitmask f =
        MR_ROBDD_PRECEDING_BITS(vr&(MR_ROBDD_BITS_PER_WORD-1));
    MR_ROBDD_bitmask *ptr = &(set->bits[wd]);
    MR_ROBDD_bitmask bits = *ptr;
    MR_ROBDD_bitmask msk = *mask;

    if ((bits&f) == 0) {
        do {
            if (--wd < 0) {
                return MR_FALSE;
            }
        } while ((bits=*--ptr) == 0);
        vr = (wd<<MR_ROBDD_LOG_BITS_PER_WORD) + MR_ROBDD_BITS_PER_WORD-1;
        msk = 1<<(MR_ROBDD_BITS_PER_WORD-1);
    }
    /* I know there's an earlier bit set in bits, so this is safe */
    while ((bits&msk) == 0) {
        --vr;
        msk >>= 1;
        assert(vr >= 0);
    }
    *var = vr;
    *word = wd;
    *mask = msk;
    return MR_TRUE;
}

#else /* !MR_ROBDD_NO_CHEAP_SHIFT */

__inline MR_ROBDD_int
MR_ROBDD_prev_element(MR_ROBDD_bitset *set, MR_ROBDD_int *var,
    MR_ROBDD_int *word, MR_ROBDD_bitmask *mask)
{
    MR_ROBDD_int vr = *var;
    MR_ROBDD_int wd = *word;
    MR_ROBDD_bitmask *ptr = &(set->bits[wd]);
    MR_ROBDD_bitmask bits =
        *ptr&MR_ROBDD_PRECEDING_BITS(vr&(MR_ROBDD_BITS_PER_WORD-1));
    MR_ROBDD_bitmask temp;

    assert(vr >= 0 && vr < MR_ROBDD_MAXVAR);

    while (bits == 0) {
        if (--wd < 0) {
            return MR_FALSE;
        }
        bits = *--ptr;
    }

    vr = MR_ROBDD_BITS_PER_WORD - MR_ROBDD_BITS_PER_CHAR;
    /* I know there's an earlier bit set in bits, so this is safe */
    while ((temp = ((bits >> vr) & MR_ROBDD_CHAR_MASK)) == 0) {
        vr -= MR_ROBDD_BITS_PER_CHAR;
        assert(vr >= 0);
    }
    vr += (MR_ROBDD_int) MR_ROBDD_last_one_bit[(MR_ROBDD_int) temp];
    vr += (MR_ROBDD_int) wd << MR_ROBDD_LOG_BITS_PER_WORD;

    *var = vr;
    *word = wd;
    *mask = 1 << (vr & (MR_ROBDD_BITS_PER_WORD-1));
    return MR_TRUE;
}

#endif /* MR_ROBDD_NO_CHEAP_SHIFT */

__inline MR_ROBDD_int
MR_ROBDD_next_nonelement(MR_ROBDD_bitset *set, MR_ROBDD_int *var,
    MR_ROBDD_int *word, MR_ROBDD_bitmask *mask)
{
    MR_ROBDD_int vr = *var;
    MR_ROBDD_int wd = *word;
    MR_ROBDD_bitmask f =
        MR_ROBDD_FOLLOWING_BITS(vr&(MR_ROBDD_BITS_PER_WORD-1));
    MR_ROBDD_bitmask *ptr = &(set->bits[wd]);
    MR_ROBDD_bitmask bits = *ptr;
    MR_ROBDD_bitmask msk = *mask;

    assert(vr >= 0 && vr < MR_ROBDD_MAXVAR);

    if ((bits&f) == f) {
        do {
            if (++wd >= MR_ROBDD_MAXVAR/MR_ROBDD_BITS_PER_WORD) {
                return MR_FALSE;
            }
        } while ((bits=*++ptr) == ~0);
        vr = wd<<MR_ROBDD_LOG_BITS_PER_WORD;
        msk = 1;
    }
    /* I know there's a later bit clear in bits, so this is safe */
    while ((bits&msk) != 0) {
        ++vr;
        msk <<= 1;
    }
    *var = vr;
    *word = wd;
    *mask = msk;
    return MR_TRUE;
}

__inline MR_ROBDD_int
MR_ROBDD_prev_nonelement(MR_ROBDD_bitset *set, MR_ROBDD_int *var,
    MR_ROBDD_int *word, MR_ROBDD_bitmask *mask)
{
    MR_ROBDD_int vr = *var;
    MR_ROBDD_int wd = *word;
    MR_ROBDD_bitmask f =
        MR_ROBDD_PRECEDING_BITS(vr&(MR_ROBDD_BITS_PER_WORD-1));
    MR_ROBDD_bitmask *ptr = &(set->bits[wd]);
    MR_ROBDD_bitmask bits = *ptr;
    MR_ROBDD_bitmask msk = *mask;

    assert(vr >= 0 && vr < MR_ROBDD_MAXVAR);

    if ((bits&f) == f) {
        do {
            if (--wd < 0) {
                return MR_FALSE;
            }
        } while ((bits=*--ptr) == ~0);
        vr = (wd<<MR_ROBDD_LOG_BITS_PER_WORD) + MR_ROBDD_BITS_PER_WORD-1;
        msk = 1<<(MR_ROBDD_BITS_PER_WORD-1);
    }
    /* I know there's an earlier bit clear in bits, so this is safe */
    while ((bits&msk) != 0) {
        --vr;
        msk >>= 1;
    }
    *var = vr;
    *word = wd;
    *mask = msk;
    return MR_TRUE;
}

/* returns 1 if set1 is identical to set2 */
__inline MR_ROBDD_int
MR_ROBDD_bitset_equal(MR_ROBDD_bitset *set1, MR_ROBDD_bitset *set2);

__inline MR_ROBDD_int
MR_ROBDD_bitset_equal(MR_ROBDD_bitset *set1, MR_ROBDD_bitset *set2)
{
    MR_ROBDD_bitmask *ptr1 = &set1->bits[0];
    MR_ROBDD_bitmask *ptr2 = &set2->bits[0];
    MR_ROBDD_bitmask *ptr1end =
        &set1->bits[((MR_ROBDD_MAXVAR-1)/MR_ROBDD_BITS_PER_WORD)+1];

    for (;;) {
        if (*ptr1 != *ptr2) {
            return 0;
        }
        if (++ptr1 >= ptr1end) {
            return 1;
        }
        ++ptr2;
    }
}

/* returns 1 if 2 sets are disjoint, else 0 */
__inline MR_ROBDD_int
MR_ROBDD_bitset_disjoint(MR_ROBDD_bitset *set1, MR_ROBDD_bitset *set2);

__inline MR_ROBDD_int
MR_ROBDD_bitset_disjoint(MR_ROBDD_bitset *set1, MR_ROBDD_bitset *set2)
{
    MR_ROBDD_bitmask *ptr1 = &set1->bits[0];
    MR_ROBDD_bitmask *ptr2 = &set2->bits[0];
    MR_ROBDD_bitmask *ptr1end =
        &set1->bits[((MR_ROBDD_MAXVAR-1)/MR_ROBDD_BITS_PER_WORD)+1];

    for (;;) {
        if ((*ptr1 & *ptr2) != 0) {
            return 0;
        }
        if (++ptr1 >= ptr1end) {
            return 1;
        }
        ++ptr2;
    }
}

/* returns 1 if set1 is a subset of set2 */
__inline MR_ROBDD_int
MR_ROBDD_bitset_subset(MR_ROBDD_bitset *set1, MR_ROBDD_bitset *set2);

__inline MR_ROBDD_int
MR_ROBDD_bitset_subset(MR_ROBDD_bitset *set1, MR_ROBDD_bitset *set2)
{
    MR_ROBDD_bitmask *ptr1 = &set1->bits[0];
    MR_ROBDD_bitmask *ptr2 = &set2->bits[0];
    MR_ROBDD_bitmask *ptr1end =
        &set1->bits[((MR_ROBDD_MAXVAR-1)/MR_ROBDD_BITS_PER_WORD)+1];

    for (;;) {
        if ((*ptr1 | *ptr2) != *ptr2) {
            return 0;
        }
        if (++ptr1 >= ptr1end) {
            return 1;
        }
        ++ptr2;
    }
    
}

/* returns 1 if set1 is a subset of set2 */
__inline MR_ROBDD_int MR_ROBDD_bitset_empty(MR_ROBDD_bitset *set);

__inline MR_ROBDD_int MR_ROBDD_bitset_empty(MR_ROBDD_bitset *set)
{
    MR_ROBDD_bitmask *ptr = &set->bits[0];
    MR_ROBDD_bitmask *ptrend =
        &set->bits[((MR_ROBDD_MAXVAR-1)/MR_ROBDD_BITS_PER_WORD)+1];

    for (;;) {
        if ((*ptr) != 0) {
            return 0;
        }
        if (++ptr >= ptrend) {
            return 1;
        }
    }
}

/****************************************************************

                 Making Nodes

 ****************************************************************/

#if defined(MR_ROBDD_BRYANT_CONSERVATIVE_GC)

static MR_ROBDD_int MR_ROBDD_removed_nodes = 0;

void MR_ROBDD_remove_node(void *obj, void *client_data);
void MR_ROBDD_remove_node(void *obj, void *client_data)
{
    MR_ROBDD_node *n = (MR_ROBDD_node *) obj;
    MR_ROBDD_BRYANT_hidden_node_pointer *bucket =
          (MR_ROBDD_BRYANT_hidden_node_pointer *) client_data;

    if (MR_ROBDD_REVEAL_NODE_POINTER(n->unique) != NULL) {
        MR_ROBDD_REVEAL_NODE_POINTER(n->unique)->uprev = n->uprev;
    }

    if (MR_ROBDD_REVEAL_NODE_POINTER(n->uprev) != NULL) {
        MR_ROBDD_REVEAL_NODE_POINTER(n->uprev)->unique = n->unique;
    }

    if (MR_ROBDD_REVEAL_NODE_POINTER(*bucket) == n) {
        *bucket = n->unique;
    }

    MR_ROBDD_removed_nodes++;
}

#endif /* MR_ROBDD_BRYANT_CONSERVATIVE_GC */

#if defined(MR_ROBDD_POOL)
  static pool           *MR_ROBDD_curr_pool = NULL;
  static MR_ROBDD_node  *MR_ROBDD_curr_pool_ptr = NULL;
  static MR_ROBDD_node  *MR_ROBDD_curr_pool_end_ptr = NULL;
#endif /* MR_ROBDD_POOL */

static MR_ROBDD_int        MR_ROBDD_node_count = 0;

static MR_ROBDD_node *
MR_ROBDD_alloc_node(MR_ROBDD_int value, MR_ROBDD_node* tr, MR_ROBDD_node* fa,
    MR_ROBDD_BRYANT_hidden_node_pointer *bucket)
{
    MR_ROBDD_node *n;
#if defined(MR_ROBDD_POOL)
    pool *newpool;

    if (MR_ROBDD_curr_pool_ptr >= MR_ROBDD_curr_pool_end_ptr) {
        /* allocate a new pool */
        newpool = (pool *) malloc(sizeof(pool));
        newpool->prev = MR_ROBDD_curr_pool;
        MR_ROBDD_curr_pool = newpool;
        MR_ROBDD_curr_pool_ptr = &(newpool->data[0]);
        MR_ROBDD_curr_pool_end_ptr = &(newpool->data[MR_ROBDD_POOL_SIZE]);
    }
    n = MR_ROBDD_curr_pool_ptr++;
#else /* !MR_ROBDD_POOL */
    n = (MR_ROBDD_node *) malloc(sizeof(MR_ROBDD_node));
  #if defined(MR_ROBDD_BRYANT_CONSERVATIVE_GC)
    GC_register_finalizer(n, MR_ROBDD_remove_node, bucket, 0, 0);
  #endif
#endif /* MR_ROBDD_POOL */
    MR_ROBDD_node_count++;
    n->value = value;
    n->tr = tr;
    n->fa = fa;
    return n;
}

/* return the number of graph nodes that have been created. */
MR_ROBDD_int
MR_ROBDD_nodes_in_use(void)
{
    return MR_ROBDD_node_count;
}

MR_ROBDD_DECLARE_FN_COUNT(MR_ROBDD_make_node)

MR_ROBDD_node *
MR_ROBDD_make_node(MR_ROBDD_int var, MR_ROBDD_node *tr, MR_ROBDD_node *fa)
{
    MR_ROBDD_BRYANT_hidden_node_pointer *bucket;
    MR_ROBDD_node *ptr;

    assert(var>=0);
    assert(var<MR_ROBDD_MAXVAR);
    assert(MR_ROBDD_IS_TERMINAL(tr) || tr->value > var);
    assert(MR_ROBDD_IS_TERMINAL(fa) || fa->value > var);

    MR_ROBDD_COUNT_FN(MR_ROBDD_make_node);

    if (tr == fa) {
        return tr;
    }

    bucket = &MR_ROBDD_unique_table[MR_ROBDD_UNIQUE_HASH(var, tr, fa)];

    /*
    ** The following check avoids the need to initialise the table
    ** elements to MR_ROBDD_HIDE_POINTER(NULL).
    */
    if (*bucket == 0) {
        *bucket = MR_ROBDD_HIDE_POINTER(NULL);
    }

    ptr = MR_ROBDD_REVEAL_NODE_POINTER(*bucket);
    while (ptr != NULL && (var!=ptr->value || tr!=ptr->tr || fa!=ptr->fa)) {
        ptr = MR_ROBDD_REVEAL_NODE_POINTER(ptr->unique);
    }

    if (ptr != NULL) {
        MR_ROBDD_COUNT_UNIQUE_HIT;
        return ptr;
    }

    /* MR_ROBDD_node doesn't exist so create it and put in bucket */
    MR_ROBDD_COUNT_UNIQUE_MISS;
    ptr = MR_ROBDD_alloc_node(var, tr, fa, bucket);
    ptr->unique = *bucket;
    *bucket = MR_ROBDD_HIDE_POINTER(ptr);
#ifdef MR_ROBDD_BRYANT_CONSERVATIVE_GC
    ptr->uprev = MR_ROBDD_HIDE_POINTER(NULL);
    if (MR_ROBDD_REVEAL_NODE_POINTER(ptr->unique) != NULL) {
        MR_ROBDD_REVEAL_NODE_POINTER(ptr->unique)->uprev =
            MR_ROBDD_HIDE_POINTER(ptr);
    }
#endif
    return ptr;
}

/****************************************************************

                The Basic Algorithms

 ****************************************************************/

MR_ROBDD_int MR_ROBDD_max_variable(void);

MR_ROBDD_int
MR_ROBDD_max_variable(void)
{
    return MR_ROBDD_MAXVAR;
}

MR_ROBDD_node *
MR_ROBDD_trueVar(void)
{
    return MR_ROBDD_one;
}

MR_ROBDD_node *
MR_ROBDD_falseVar(void)
{
    return MR_ROBDD_zero;
}
    
MR_ROBDD_DECLARE_FN_COUNT(MR_ROBDD_variableRep)

MR_ROBDD_node *
MR_ROBDD_variableRep(MR_ROBDD_int var)
{
    MR_ROBDD_COUNT_FN(MR_ROBDD_variableRep);
    return MR_ROBDD_make_node(var, MR_ROBDD_one, MR_ROBDD_zero);
}

MR_ROBDD_DECLARE_FN_COUNT(MR_ROBDD_ite)

MR_ROBDD_node *
MR_ROBDD_ite(MR_ROBDD_node *f, MR_ROBDD_node *g, MR_ROBDD_node *h)
{
    MR_ROBDD_node *f_tr, *f_fa;
    MR_ROBDD_node *g_tr, *g_fa;
    MR_ROBDD_node *h_tr, *h_fa;
    MR_ROBDD_node *newnode;
    MR_ROBDD_int top;
    MR_ROBDD_DECLARE_ITE_CACHE_ENTRY

    MR_ROBDD_COUNT_FN(MR_ROBDD_ite);
    /* terminal cases */
    if (f == MR_ROBDD_one) {
      return g;
    }
    if (f == MR_ROBDD_zero) {
      return h;
    }
    if ((g == MR_ROBDD_one) && (h == MR_ROBDD_zero)) {
      return f;
    }
    if (g == h) {
      return g;
    }

    /* look it up in computed table; finished if found */
    MR_ROBDD_TRY_ITE_CACHE(f, g, h, MR_ROBDD_ite);

    /* find top variable */
    top = f->value;        /* we know f is not terminal */
    if (!MR_ROBDD_IS_TERMINAL(g) && (g->value < top)) {
        top = g->value;
    }
    if (!MR_ROBDD_IS_TERMINAL(h) && (h->value < top)) {
        top = h->value;
    }
    
    /* find then and else branches for recursive calls */
    if (f->value==top) {
        f_tr = f->tr; f_fa = f->fa;
    } else {
        f_tr = f; f_fa = f;
    }
    if (!MR_ROBDD_IS_TERMINAL(g) && g->value==top) {
        g_tr = g->tr; g_fa = g->fa;
    } else {
        g_tr = g; g_fa = g;
    }
    if (!MR_ROBDD_IS_TERMINAL(h) && h->value==top) {
        h_tr = h->tr; h_fa = h->fa;
    } else {
        h_tr = h; h_fa = h;
    }
    
    /* create new MR_ROBDD_node and add to table */
    newnode = MR_ROBDD_make_node(top,
                MR_ROBDD_ite(f_tr, g_tr, h_tr),
                MR_ROBDD_ite(f_fa, g_fa, h_fa));
    MR_ROBDD_UPDATE_ITE_CACHE(f, g, h, newnode, MR_ROBDD_ite);
    return newnode;
}

#if defined(MR_ROBDD_USE_ITE_CONSTANT)

/*
** This is sort of an "approximate MR_ROBDD_ite()."  It returns MR_ROBDD_zero
** or MR_ROBDD_one if that's what MR_ROBDD_ite() would do.  Otherwise it just
** returns the pseudo-MR_ROBDD_node `MR_ROBDD_nonterminal' or some real
** MR_ROBDD_node.  In any case, it does not create any new nodes.
*/

MR_ROBDD_node *
MR_ROBDD_ite_constant(MR_ROBDD_node *f, MR_ROBDD_node *g, MR_ROBDD_node *h)
{
    MR_ROBDD_node *f_tr, *f_fa;
    MR_ROBDD_node *g_tr, *g_fa;
    MR_ROBDD_node *h_tr, *h_fa;
    MR_ROBDD_node *tr_part, *fa_part;
    MR_ROBDD_node *result;
    MR_ROBDD_int top;
    MR_ROBDD_DECLARE_ITE_CACHE_ENTRY

    MR_ROBDD_COUNT_FN(MR_ROBDD_ite);
    /* terminal cases */
    if (f == MR_ROBDD_one) {
        return g;
    }
    if (f == MR_ROBDD_zero) {
        return h;
    }
    if (g == h) {
        return g;
    }
    if (MR_ROBDD_IS_TERMINAL(g) && MR_ROBDD_IS_TERMINAL(h)) {
        /* either f or ~f, which is MR_ROBDD_nonterminal since f is */
        return MR_ROBDD_nonterminal;
    }

    /* look it up in computed table; finished if found */
    MR_ROBDD_TRY_ITE_CACHE(f, g, h, MR_ROBDD_ite_constant);

    /* find top variable */
    top = f->value;        /* we know f is not terminal */
    if (!MR_ROBDD_IS_TERMINAL(g) && (g->value < top)) {
        top = g->value;
    }
    if (!MR_ROBDD_IS_TERMINAL(h) && (h->value < top)) {
        top = h->value;
    }
    
    /* find then and else branches for recursive calls */
    if (f->value==top) {
        f_tr = f->tr; f_fa = f->fa;
    } else {
        f_tr = f; f_fa = f;
    }
    if (!MR_ROBDD_IS_TERMINAL(g) && g->value==top) {
        g_tr = g->tr; g_fa = g->fa;
    } else {
        g_tr = g; g_fa = g;
    }
    if (!MR_ROBDD_IS_TERMINAL(h) && h->value==top) {
        h_tr = h->tr; h_fa = h->fa;
    } else {
        h_tr = h; h_fa = h;
    }
    
    tr_part = MR_ROBDD_ite_constant(f_tr, g_tr, h_tr);
    fa_part = MR_ROBDD_ite_constant(f_fa, g_fa, h_fa);
    if (tr_part == fa_part) {
        result = tr_part;
    } else {
        result = MR_ROBDD_nonterminal;
    }

    MR_ROBDD_UPDATE_ITE_CACHE(f, g, h, result, MR_ROBDD_ite_constant);
    return result;
}
#endif /* MR_ROBDD_USE_ITE_CONSTANT */

MR_ROBDD_DECLARE_FN_COUNT(MR_ROBDD_implies)

MR_ROBDD_node *
MR_ROBDD_implies(MR_ROBDD_node *a, MR_ROBDD_node *b)
{
    MR_ROBDD_COUNT_FN(MR_ROBDD_implies);
    return MR_ROBDD_ite(a, b, MR_ROBDD_one);
}

MR_ROBDD_DECLARE_FN_COUNT(MR_ROBDD_lub)
MR_ROBDD_DECLARE_FN_COUNT(MR_ROBDD_glb)

#if defined(MR_ROBDD_NAIVE)
MR_ROBDD_node *
MR_ROBDD_glb(MR_ROBDD_node *a, MR_ROBDD_node *b)
{
    MR_ROBDD_COUNT_FN(MR_ROBDD_glb);
    return MR_ROBDD_ite(a, b, MR_ROBDD_zero);
}

MR_ROBDD_node *
MR_ROBDD_lub(MR_ROBDD_node *a, MR_ROBDD_node *b)
{
    MR_ROBDD_COUNT_FN(MR_ROBDD_lub);
    return MR_ROBDD_ite(a, MR_ROBDD_one, b);
}

#else /* !MR_ROBDD_NAIVE */

MR_ROBDD_node *
MR_ROBDD_lub(MR_ROBDD_node *f, MR_ROBDD_node *g)
{
    MR_ROBDD_COUNT_FN(MR_ROBDD_lub);
    if (MR_ROBDD_IS_TERMINAL(f)) {
        return f == MR_ROBDD_one ? MR_ROBDD_one : g;
    } else if (MR_ROBDD_IS_TERMINAL(g)) {
        return g == MR_ROBDD_one ? MR_ROBDD_one : f;
    } MR_ROBDD_ELSE_TRY_EQUAL_TEST(f, g, f)
    else {
        MR_ROBDD_node *result;
        MR_ROBDD_DECLARE_BIN_CACHE_ENTRY

        MR_ROBDD_TRY_BIN_CACHE(f, g, MR_ROBDD_lub);

        if (f->value < g->value) {
            result = MR_ROBDD_make_node(f->value,
                MR_ROBDD_lub(f->tr, g), MR_ROBDD_lub(f->fa, g));
        } else if (f->value > g->value) {
            result = MR_ROBDD_make_node(g->value,
                MR_ROBDD_lub(f, g->tr), MR_ROBDD_lub(f, g->fa));
        } else /* f->value == g->value */{
            result = MR_ROBDD_make_node(f->value,
                MR_ROBDD_lub(f->tr, g->tr), MR_ROBDD_lub(f->fa, g->fa));
        }
        MR_ROBDD_UPDATE_BIN_CACHE(f, g, result, MR_ROBDD_lub);
        return result;
    }
}

MR_ROBDD_node *
MR_ROBDD_glb(MR_ROBDD_node *f, MR_ROBDD_node *g)
{
    MR_ROBDD_COUNT_FN(MR_ROBDD_glb);
    if (MR_ROBDD_IS_TERMINAL(f)) {
        return f == MR_ROBDD_one ? g : MR_ROBDD_zero;
    } else if (MR_ROBDD_IS_TERMINAL(g)) {
        return g == MR_ROBDD_one ? f : MR_ROBDD_zero;
    } MR_ROBDD_ELSE_TRY_EQUAL_TEST(f, g, f)
    else {
        MR_ROBDD_node *result;
        MR_ROBDD_DECLARE_BIN_CACHE_ENTRY

        MR_ROBDD_TRY_BIN_CACHE(f, g, MR_ROBDD_glb);

        if (f->value < g->value) {
            result = MR_ROBDD_make_node(f->value,
                MR_ROBDD_glb(f->tr, g), MR_ROBDD_glb(f->fa, g));
        } else if (f->value > g->value) {
            result = MR_ROBDD_make_node(g->value,
                MR_ROBDD_glb(f, g->tr), MR_ROBDD_glb(f, g->fa));
        } else /* f->value == g->value */{
            result = MR_ROBDD_make_node(f->value,
                MR_ROBDD_glb(f->tr, g->tr), MR_ROBDD_glb(f->fa, g->fa));
        }
        MR_ROBDD_UPDATE_BIN_CACHE(f, g, result, MR_ROBDD_glb);
        return result;
    }
}

#endif /* MR_ROBDD_NAIVE */

#if defined(MR_ROBDD_NAIVE)

MR_ROBDD_node *
MR_ROBDD_glb_array(MR_ROBDD_int n, MR_ROBDD_int arr[])
{
    MR_ROBDD_int i;
    MR_ROBDD_node *result = MR_ROBDD_one;

    for (i = 0; i < n; ++i) {
        result = MR_ROBDD_glb(result, MR_ROBDD_variableRep(arr[i]));
    }
    return result;
}

#else /* !MR_ROBDD_NAIVE */

MR_ROBDD_node *
MR_ROBDD_glb_array(MR_ROBDD_int n, MR_ROBDD_int arr[])
{
    MR_ROBDD_int i;
    MR_ROBDD_node *result = MR_ROBDD_one;

    qsort((char *)arr, n, sizeof(MR_ROBDD_int), MR_ROBDD_intcompare);
    for (i = n-1; i >= 0; --i) {
        result = MR_ROBDD_make_node(arr[i], result, MR_ROBDD_zero);
    }
    return result;
}

#endif /* MR_ROBDD_NAIVE */

/****************************************************************

     Restriction (Projection, Existential Quantification)

 ****************************************************************/

MR_ROBDD_DECLARE_FN_COUNT(MR_ROBDD_restrict)

/* restricts c in f. */
MR_ROBDD_node *
MR_ROBDD_restrict(MR_ROBDD_int c, MR_ROBDD_node *f)
{
    MR_ROBDD_COUNT_FN(MR_ROBDD_restrict);
    if (MR_ROBDD_IS_TERMINAL(f) || (f->value > c)) {
        return f;
    } else if (f->value < c) {
        return MR_ROBDD_make_node(f->value,
            MR_ROBDD_restrict(c, f->tr), MR_ROBDD_restrict(c, f->fa));
    } else {
        return MR_ROBDD_lub(f->tr, f->fa);
    }
}

MR_ROBDD_DECLARE_FN_COUNT(MR_ROBDD_restrictThresh)

#if !defined(MR_ROBDD_USE_THRESH) && !defined(MR_ROBDD_RESTRICT_SET)
MR_ROBDD_node *
MR_ROBDD_restrictThresh(MR_ROBDD_int lo, MR_ROBDD_int hi, MR_ROBDD_node *a)
{
    MR_ROBDD_int i;

    MR_ROBDD_COUNT_FN(MR_ROBDD_restrictThresh);
    for(i = lo+1; i <= hi; ++i) {
        a = MR_ROBDD_restrict(i, a);
    }
    return a;
}

#elif !defined(MR_ROBDD_USE_THRESH) /* && defined(MR_ROBDD_RESTRICT_SET) */

/*
** union vars with the set of all variables greater than thresh appearing in
** the ROBDD rooted at f.
*/

static void
MR_ROBDD_vars_present(MR_ROBDD_node *f, MR_ROBDD_int thresh,
    MR_ROBDD_bitset *vars)
{
    while (!MR_ROBDD_IS_TERMINAL(f)) {
        if (f->value > thresh) {
            MR_ROBDD_BITSET_ADD_ELEMENT(*vars, f->value);
        }
        MR_ROBDD_vars_present(f->tr, thresh, vars);
        f = f->fa;
    }
}

MR_ROBDD_node *
MR_ROBDD_restrictThresh(MR_ROBDD_int thresh, MR_ROBDD_node *f)
{
    MR_ROBDD_bitset vars;
    MR_ROBDD_int var;
    MR_ROBDD_int word;
    MR_ROBDD_bitmask mask;

    MR_ROBDD_COUNT_FN(MR_ROBDD_restrictThresh);
    MR_ROBDD_BITSET_CLEAR(vars);
    MR_ROBDD_vars_present(f, thresh, &vars);
    MR_ROBDD_REV_FOREACH_ELEMENT(vars, var, word, mask) {
        f = MR_ROBDD_restrict(var, f);
    }
    return f;
}

#else /* MR_ROBDD_USE_THRESH */

MR_ROBDD_node *
MR_ROBDD_restrictThresh(MR_ROBDD_int thresh, MR_ROBDD_node *f)
{
    /* restricts all variables greater than thresh. */

    MR_ROBDD_COUNT_FN(MR_ROBDD_restrictThresh);
    if (MR_ROBDD_IS_TERMINAL(f)) {
        return f;
    } else if (f->value <= thresh) {
        MR_ROBDD_node *result;        
        MR_ROBDD_DECLARE_ASYM_BIN_CACHE_ENTRY;

        MR_ROBDD_TRY_ASYM_BIN_CACHE((MR_ROBDD_node *) thresh, f,
            MR_ROBDD_restrictThresh);

        result = MR_ROBDD_make_node(f->value,
                 MR_ROBDD_restrictThresh(thresh, f->tr),
                 MR_ROBDD_restrictThresh(thresh, f->fa));
        MR_ROBDD_UPDATE_ASYM_BIN_CACHE((MR_ROBDD_node *) thresh, f, result,
            MR_ROBDD_restrictThresh);
        return result;
    } else {
        return MR_ROBDD_one;
    }
}

#endif /* MR_ROBDD_USE_THRESH */

#if !defined(MR_ROBDD_USE_THRESH) && !defined(MR_ROBDD_RESTRICT_SET)

MR_ROBDD_node *
MR_ROBDD_restricted_glb(MR_ROBDD_int lo, MR_ROBDD_int hi,
    MR_ROBDD_node *f, MR_ROBDD_node *g)
{
    return MR_ROBDD_restrictThresh(lo, hi, MR_ROBDD_glb(f, g));
}

#elif !defined(MR_ROBDD_USE_RGLB) /* && ( MR_ROBDD_USE_THRESH || MR_ROBDD_RESTRICT_SET ) */

MR_ROBDD_node *
MR_ROBDD_restricted_glb(MR_ROBDD_int thresh,
    MR_ROBDD_node *f, MR_ROBDD_node *g)
{
    return MR_ROBDD_restrictThresh(thresh, MR_ROBDD_glb(f, g));
}

#else /* MR_ROBDD_USE_RGLB */

/* returns true iff MR_ROBDD_glb(f, g) is not 'MR_ROBDD_zero' */
static MR_ROBDD_int
MR_ROBDD_exists_glb(MR_ROBDD_node *f, MR_ROBDD_node *g)
{
    if (f == MR_ROBDD_zero) {
        return MR_FALSE;
    } else if (g == MR_ROBDD_zero) {
        return MR_FALSE;
    } else if (f == MR_ROBDD_one) {
        /* since we know that g != MR_ROBDD_zero... */
        return MR_TRUE;
    } else if (g == MR_ROBDD_one) {
        /* likewise f... */
        return MR_TRUE;
    } else if (f->value < g->value) {
        return MR_ROBDD_exists_glb(f->tr, g) || MR_ROBDD_exists_glb(f->fa, g);
    } else if (f->value > g->value) {
        return MR_ROBDD_exists_glb(f, g->tr) || MR_ROBDD_exists_glb(f, g->fa);
    } else {
        return MR_ROBDD_exists_glb(f->tr, g->tr) || MR_ROBDD_exists_glb(f->fa, g->fa);
    }
}

MR_ROBDD_node *
MR_ROBDD_restricted_glb(MR_ROBDD_int c, MR_ROBDD_node *f, MR_ROBDD_node *g)
{
    if (MR_ROBDD_IS_TERMINAL(f)) {
        return (f == MR_ROBDD_one) ? MR_ROBDD_restrictThresh(c, g)
            : MR_ROBDD_zero;
    } else if (MR_ROBDD_IS_TERMINAL(g)) {
        return (g == MR_ROBDD_one) ? MR_ROBDD_restrictThresh(c, f)
            : MR_ROBDD_zero;
    } MR_ROBDD_ELSE_TRY_EQUAL_TEST(f, g, MR_ROBDD_restrictThresh(c, f))
    else {
        MR_ROBDD_int v;
        MR_ROBDD_node *tr1, *tr2, *fa1, *fa2;
        MR_ROBDD_node *result;

        MR_ROBDD_DECLARE_RGLB_CACHE_ENTRY

        MR_ROBDD_TRY_RGLB_CACHE(f, g, c);

        if (f->value < g->value) {
            v = f->value;
            tr1 = f->tr;
            tr2 = g;
            fa1 = f->fa;
            fa2 = g;
        } else if (f->value > g->value) {
            v = g->value;
            tr1 = f;
            tr2 = g->tr;
            fa1 = f;
            fa2 = g->fa;
        } else /* f->value == g->value */{
            v = f->value;
            tr1 = f->tr;
            tr2 = g->tr;
            fa1 = f->fa;
            fa2 = g->fa;
        }
        if (v > c) {
            result =
                (MR_ROBDD_exists_glb(tr1, tr2) || MR_ROBDD_exists_glb(fa1, fa2)) ?
                    MR_ROBDD_one : MR_ROBDD_zero;
        } else {
            result = MR_ROBDD_make_node(v,
               MR_ROBDD_restricted_glb(c, tr1, tr2),
               MR_ROBDD_restricted_glb(c, fa1, fa2));
        }
        MR_ROBDD_UPDATE_RGLB_CACHE(f, g, c, result);
        return result;
    }
}
#endif /* MR_ROBDD_USE_THRESH */

/****************************************************************

                   Renaming

 ****************************************************************/

MR_ROBDD_DECLARE_FN_COUNT(MR_ROBDD_renameArray)
MR_ROBDD_DECLARE_FN_COUNT(MR_ROBDD_reverseRenameArray)

#if defined(MR_ROBDD_NEW)

MR_ROBDD_DECLARE_FN_COUNT(MR_ROBDD_ite_var)

/* A special case version of MR_ROBDD_ite_var, where we know that f < g->value */
static MR_ROBDD_node *
MR_ROBDD_ite_var_g(MR_ROBDD_int f, MR_ROBDD_node *g, MR_ROBDD_node *h)
{
    MR_ROBDD_COUNT_FN(MR_ROBDD_ite_var);

    assert(MR_ROBDD_IS_TERMINAL(g) || f < g->value);

    if (MR_ROBDD_IS_TERMINAL(h) || f < h->value) {
        /* f < g && f < h */
        return MR_ROBDD_make_node(f, g, h);
    } else if (f == h->value) {
        return MR_ROBDD_make_node(f, g, h->fa);
    } else {
        /* h < f < g */
        MR_ROBDD_node *result;
        MR_ROBDD_DECLARE_ITE_VAR_CACHE_ENTRY

        MR_ROBDD_TRY_ITE_VAR_CACHE(f, g, h);

        result = MR_ROBDD_make_node(h->value,
                   MR_ROBDD_ite_var_g(f, g, h->tr),
                   MR_ROBDD_ite_var_g(f, g, h->fa));
        MR_ROBDD_UPDATE_ITE_VAR_CACHE(f, g, h, result);
        return result;
    }
}

/*
** A special case version of MR_ROBDD_ite_var, where we know that
** f < h->value
*/

static MR_ROBDD_node *
MR_ROBDD_ite_var_h(MR_ROBDD_int f, MR_ROBDD_node *g, MR_ROBDD_node *h)
{
    MR_ROBDD_COUNT_FN(MR_ROBDD_ite_var);

    assert(MR_ROBDD_IS_TERMINAL(h) || f < h->value);

    if (MR_ROBDD_IS_TERMINAL(g) || f < g->value) {
        /* f < g && f < h */
        return MR_ROBDD_make_node(f, g, h);
    } else if (f == g->value) {
        return MR_ROBDD_make_node(f, g->tr, h);
    } else {
        /* h < f < g */
        MR_ROBDD_node *result;
        MR_ROBDD_DECLARE_ITE_VAR_CACHE_ENTRY

        MR_ROBDD_TRY_ITE_VAR_CACHE(f, g, h);
        result = MR_ROBDD_make_node(g->value,
                   MR_ROBDD_ite_var_h(f, g->tr, h),
                   MR_ROBDD_ite_var_h(f, g->fa, h));
        MR_ROBDD_UPDATE_ITE_VAR_CACHE(f, g, h, result);
        return result;
    }
}

/*
** A special case version of MR_ROBDD_ite, where we know that
** !MR_ROBDD_IS_TERMINAL(f) && f->tr == MR_ROBDD_one && f->fa == MR_ROBDD_zero.
** In fact, we refine this further and make f be just (what would have been)
** f->value.
**
** Recall the code for MR_ROBDD_ite: it finds the minimum value of f, g, and h
** (call it top), and sets each of ft, gt and ht to the tr branch of the
** corresponding arg if its value == top, else sets it to the arg itself, and
** correspondingly for ff, gf, and hf.  Then the value of MR_ROBDD_ite is:
**
**    mn(top, i(ft, gt, ht), i(ff, gf, hf))
**
** (abbreviating MR_ROBDD_make_node as mn and MR_ROBDD_ite as i). Given this,
** we can simplify things in several cases. Here are all the cases, and the
** simplified value. (We introduce ig for MR_ROBDD_ite_var_g and ih for
** MR_ROBDD_ite_var_h as special cases where we know f < g or f < h,
** respectively.)
**
** a)    f = g = h   (1)   mn(f, g->tr, h->fa) *** Impossible
**
** b)    f < g < h   (2)   mn(f, g, h)
** c)    f < g = h   (2)   mn(f, g, h)
** d)    f < h < g   (2)   mn(f, g, h)
** e)    f = g < h   (3)   mn(f, g->tr, h) *** Impossible
** f)    f = h < g   (4)   mn(f, g, h->fa) *** Impossible
**
** g)    g < f < h   (5)   mn(gv, ih(f, g->tr, h), ih(f, g->fa, h))
** h)    g < f = h   (6)   mn(gv, i(f, g->tr, h), i(f, g->fa, h)) *** Impossible
** i)    g < h < f   (6)   mn(gv, i(f, g->tr, h), i(f, g->fa, h))
** j)    g = h < f   (7)   mn(gv, i(f, g->tr, h->tr), i(f, g->fa, h->fa))
**
** k)    h < f < g   (8)   mn(hv, ig(f, g, h->tr), ig(f, g, h->fa))
** l)    h < f = g   (9)   mn(hv, i(f, g, h->tr), i(f, g, h->fa)) *** Impossible
** m)    h < g < f   (9)   mn(hv, i(f, g, h->tr), i(f, g, h->fa))
*/        

MR_ROBDD_node *
MR_ROBDD_ite_var(MR_ROBDD_int f, MR_ROBDD_node *g, MR_ROBDD_node *h)
{
    MR_ROBDD_int g_val = MR_ROBDD_int_max;
    MR_ROBDD_int h_val = MR_ROBDD_int_max;
    MR_ROBDD_node *result;
    MR_ROBDD_node *g_tr, *g_fa, *h_tr, *h_fa;
    MR_ROBDD_DECLARE_ITE_VAR_CACHE_ENTRY

    MR_ROBDD_COUNT_FN(MR_ROBDD_ite_var);
    MR_ROBDD_DIRECT_EQUAL_TEST(g, h, g);
    MR_ROBDD_TRY_ITE_VAR_CACHE(f, g, h);

    if (!MR_ROBDD_IS_TERMINAL(g)) g_val = g->value;
    if (!MR_ROBDD_IS_TERMINAL(h)) h_val = h->value;

    if (MR_ROBDD_IS_TERMINAL(g)) {
        g_tr = g_fa = g;
    } else {
        g_tr = g->tr;
        g_fa = g->fa;
    }
    if (MR_ROBDD_IS_TERMINAL(h)) {
        h_tr = h_fa = h;
    } else {
        h_tr = h->tr;
        h_fa = h->fa;
    }

    if (f < g_val) {
        if (f < h_val) /* case 2 (b, c, d):  f < g && f < h */ {
            result = MR_ROBDD_make_node(f, g, h);
        } else /* case 8 (k):  h < f < g */ {
            result = MR_ROBDD_make_node(h_val,
                   MR_ROBDD_ite_var_g(f, g, h_tr),
                   MR_ROBDD_ite_var_g(f, g, h_fa));
        }
    /* g < f */
    } else if (f < h_val) /* g < f < h */ {
        result = MR_ROBDD_make_node(g_val,
                   MR_ROBDD_ite_var_h(f, g_tr, h),
                   MR_ROBDD_ite_var_h(f, g_fa, h));
    /* g < f && h < f */
    } else if (h_val < g_val) /* case 9 (l, m): h < g < f */ {
        result = MR_ROBDD_make_node(h_val,
                   MR_ROBDD_ite_var(f, g, h_tr),
                   MR_ROBDD_ite_var(f, g, h_fa));
    /* g < f && g <= h */
    } else if (g_val == h_val) /* g == h < f */ {
        result = MR_ROBDD_make_node(g_val,
                   MR_ROBDD_ite_var(f, g_tr, h_tr),
                   MR_ROBDD_ite_var(f, g_fa, h_fa));
    } else /* case 6 (h, i):  g < h < f */ {
        result = MR_ROBDD_make_node(g_val,
                   MR_ROBDD_ite_var(f, g_tr, h),
                   MR_ROBDD_ite_var(f, g_fa, h));
    }

    MR_ROBDD_UPDATE_ITE_VAR_CACHE(f, g, h, result);
    return result;
}

#else /* ! MR_ROBDD_NEW */

#define MR_ROBDD_ite_var(v, f, g) MR_ROBDD_ite(MR_ROBDD_variableRep(v), f, g)

#endif /* !MR_ROBDD_NEW */

MR_ROBDD_node *
MR_ROBDD_renameArray(MR_ROBDD_node *f, MR_ROBDD_int count,
    MR_ROBDD_int mapping[])
{
    MR_ROBDD_int newval;

    MR_ROBDD_COUNT_FN(MR_ROBDD_renameArray);
    if (MR_ROBDD_IS_TERMINAL(f)) {
        return f;
    } else if (f->value > count) {
        return MR_ROBDD_one;
    } else if (MR_ROBDD_UNUSED_MAPPING==(newval=mapping[f->value])) {
        return MR_ROBDD_lub(MR_ROBDD_renameArray(f->tr, count, mapping),
               MR_ROBDD_renameArray(f->fa, count, mapping));
    } else {
        return MR_ROBDD_ite_var(newval,
               MR_ROBDD_renameArray(f->tr, count, mapping),
               MR_ROBDD_renameArray(f->fa, count, mapping));
    }
}

MR_ROBDD_node *
MR_ROBDD_reverseRenameArray(MR_ROBDD_node *f, MR_ROBDD_int count,
    MR_ROBDD_int mapping[])
{
    MR_ROBDD_int i, val, MR_ROBDD_max;
    MR_ROBDD_int rev_map[MR_ROBDD_MAXVAR];

    MR_ROBDD_COUNT_FN(MR_ROBDD_reverseRenameArray);
    /* NB:  four -1 bytes is the same as a -1 word */
    MR_memset(rev_map, ~((char) 0), sizeof(rev_map));
    for (i=1, MR_ROBDD_max=-1; i<=count; ++i) {
        rev_map[(val=mapping[i])] = i;
        if (MR_ROBDD_max < val) MR_ROBDD_max = val;
    }

    return MR_ROBDD_renameArray(f, MR_ROBDD_max, rev_map);
}

/****************************************************************

     Abstract Exit (renaming + conjunction + restriction)

 ****************************************************************/

#if !defined(MR_ROBDD_USE_THRESH) && !defined(MR_ROBDD_RESTRICT_SET)

MR_ROBDD_node *
MR_ROBDD_abstract_exit(MR_ROBDD_node *context, MR_ROBDD_node *f,
    MR_ROBDD_int count, MR_ROBDD_int mapping[],
    MR_ROBDD_int lo, MR_ROBDD_int hi)
{
    return MR_ROBDD_restricted_glb(lo, hi, context,
        MR_ROBDD_renameArray(f, count, mapping));
}

#elif 1 /* ( MR_ROBDD_USE_THRESH || MR_ROBDD_RESTRICT_SET ) */

MR_ROBDD_node *
MR_ROBDD_abstract_exit(MR_ROBDD_node *context, MR_ROBDD_node *f,
    MR_ROBDD_int count, MR_ROBDD_int mapping[], MR_ROBDD_int thresh)
{
    return MR_ROBDD_restricted_glb(thresh, context,
        MR_ROBDD_renameArray(f, count, mapping));
}

#else /* 0 (this code not used) */

/*
** returns MR_FALSE iff the function f restricted so that all variables in
** trues are set to true and all in falses are set to false evaluates to
** MR_ROBDD_zero.
*/

int
MR_ROBDD_exists_restrict_sets(MR_ROBDD_node *f, MR_ROBDD_bitset *trues,
    MR_ROBDD_bitset *falses, MR_ROBDD_int hielt)
{
    for (;;) {
        if (MR_ROBDD_IS_TERMINAL(f)) {
            return f == MR_ROBDD_one;
        } else if (f->value > hielt) {
            return MR_TRUE;
        } else {
            MR_ROBDD_int word = MR_ROBDD_BITSET_WORD(f->value);
            MR_ROBDD_bitmask mask = MR_ROBDD_BITSET_MASK(f->value);

            if (MR_ROBDD_BITSET_MEMBER(*trues, word, mask)) {
                f = f->tr;            /* continue loop */
            } else if (MR_ROBDD_BITSET_MEMBER(*falses, word, mask)) {
                f = f->fa;            /* continue loop */
            } else if (MR_ROBDD_exists_restrict_sets(f->tr, trues, falses,
                hielt))
            {
                return MR_TRUE;
            } else {
                f = f->fa;
            }
        }
    }
}

/*
** computes the function f restricted so that all variables in trues
** are set to true and all in falses are set to false.
*/

MR_ROBDD_node *
MR_ROBDD_restrict_sets(MR_ROBDD_node *f, MR_ROBDD_bitset *trues,
    MR_ROBDD_bitset *falses, MR_ROBDD_int hielt, MR_ROBDD_int thresh)
{
    for (;;) {
        if (MR_ROBDD_IS_TERMINAL(f)) {
            return f;
        } else if (f->value > hielt) {
            return MR_ROBDD_restrictThresh(thresh, f);
        } else if (f->value > thresh) {
            return MR_ROBDD_exists_restrict_sets(f, trues, falses, hielt)
                ? MR_ROBDD_one : MR_ROBDD_zero;
        } else {
            MR_ROBDD_int word = MR_ROBDD_BITSET_WORD(f->value);
            MR_ROBDD_bitmask mask = MR_ROBDD_BITSET_MASK(f->value);

            if (MR_ROBDD_BITSET_MEMBER(*trues, word, mask)) {
                f = f->tr;            /* continue loop */
            } else if (MR_ROBDD_BITSET_MEMBER(*falses, word, mask)) {
                f = f->fa;            /* continue loop */
            } else {
                return MR_ROBDD_make_node(f->value,
                         MR_ROBDD_restrict_sets(f->tr, trues, falses, hielt,
                               thresh),
                         MR_ROBDD_restrict_sets(f->fa, trues, falses, hielt,
                               thresh));
            }
        }
    }
}

/*
** if f->value > thresh, returns MR_ROBDD_one, else if f->value is in either
** trues (or falses), this returns
**    MR_ROBDD_follow_path(f->tr (or fa), trues, falses}
** else it returns f.
*/

MR_ROBDD_node *
MR_ROBDD_follow_path(MR_ROBDD_node *f, MR_ROBDD_bitset *trues,
    MR_ROBDD_bitset *falses)
{

    while (!MR_ROBDD_IS_TERMINAL(f)) {
        MR_ROBDD_int word = MR_ROBDD_BITSET_WORD(f->value);
        MR_ROBDD_bitmask mask = MR_ROBDD_BITSET_MASK(f->value);
        
        if (MR_ROBDD_BITSET_MEMBER(*trues, word, mask)) {
            f = f->tr;
        } else if (MR_ROBDD_BITSET_MEMBER(*falses, word, mask)) {
            f = f->fa;
        } else {
            break;
        }
    }
    return f;
}

/*
** This function computes
**
**    MR_ROBDD_restrictThresh(MR_ROBDD_renameArray(f, count, mapping), thresh)
**
*/

MR_ROBDD_node *
MR_ROBDD_restricted_rename(MR_ROBDD_node *f, MR_ROBDD_int count,
    MR_ROBDD_int mapping[], MR_ROBDD_int thresh)
{
    if (MR_ROBDD_IS_TERMINAL(f)) {
        return f;
    } else {
        MR_ROBDD_int newval = mapping[f->value];
        MR_ROBDD_node *tr, *fa;

        tr = MR_ROBDD_restricted_rename(f->tr, count, mapping, thresh);
        fa = MR_ROBDD_restricted_rename(f->fa, count, mapping, thresh);

        if (newval > thresh) {
            return MR_ROBDD_lub(tr, fa);
        } else {
            return MR_ROBDD_ite_var(newval, tr, fa);
        }
    }
}

/*
** This function computes
**
**    MR_ROBDD_restrictThresh(MR_ROBDD_glb(context1,
**        MR_ROBDD_renameArray(f, count, mapping)), thresh)
**
** where context1 is context restricted by the settings in trues and
** falses.  We know that !MR_ROBDD_IS_TERMINAL(context).
*/

MR_ROBDD_node *
MR_ROBDD_abexit(MR_ROBDD_node *context, MR_ROBDD_node *f, MR_ROBDD_int count,
    MR_ROBDD_int mapping[], MR_ROBDD_int thresh, MR_ROBDD_bitset *trues,
    MR_ROBDD_bitset *falses, MR_ROBDD_int hielt)
{
    if (MR_ROBDD_IS_TERMINAL(f)) {
        if (f==MR_ROBDD_one) {
            return MR_ROBDD_restrict_sets(context, trues, falses, hielt,
                thresh);
        } else {
            return f;
        }
    } else {
        MR_ROBDD_int newval = mapping[f->value];
        MR_ROBDD_node *tr, *fa;

        if (newval == context->value) {
            tr = MR_ROBDD_follow_path(context->tr, trues, falses);
            fa = MR_ROBDD_follow_path(context->fa, trues, falses);

            if (tr == MR_ROBDD_one) {
                tr = MR_ROBDD_restricted_rename(f->tr, count, mapping, thresh);
            } else if (tr != MR_ROBDD_zero) {
                tr = MR_ROBDD_abexit(tr, f->tr, count, mapping, thresh,
                    trues, falses, hielt);
            }
            if (fa == MR_ROBDD_one) {
                fa = MR_ROBDD_restricted_rename(f->fa, count, mapping, thresh);
            } else if (fa != MR_ROBDD_zero) {
                fa = MR_ROBDD_abexit(fa, f->fa, count, mapping, thresh,
                    trues, falses, hielt);
            }
        } else {
            MR_ROBDD_int word = MR_ROBDD_BITSET_WORD(newval);
            MR_ROBDD_bitmask mask = MR_ROBDD_BITSET_MASK(newval);
            MR_ROBDD_int newhi = (newval>hielt ? newval : hielt);

            MR_ROBDD_BITSET_ADD(*trues, word, mask); /* turn on true bit */
            tr = MR_ROBDD_abexit(context, f->tr, count, mapping, thresh,
                    trues, falses, newhi);
            MR_ROBDD_BITSET_TOGGLE(*trues, word, mask); /* turn off true bit */
            MR_ROBDD_BITSET_ADD(*falses, word, mask); /* turn on false bit */
            fa = MR_ROBDD_abexit(context, f->fa, count, mapping, thresh,
                    trues, falses, newhi);
            MR_ROBDD_BITSET_TOGGLE(*falses, word, mask); /* turn off false bit */
        }
        if (newval > thresh) {
            return MR_ROBDD_lub(tr, fa);
        } else {
            return MR_ROBDD_ite_var(newval, tr, fa);
        }
    }
}

MR_ROBDD_node *
MR_ROBDD_abstract_exit(MR_ROBDD_node *context, MR_ROBDD_node *f,
    MR_ROBDD_int count, MR_ROBDD_int mapping[], MR_ROBDD_int thresh)
{
    MR_ROBDD_bitset trues;
    MR_ROBDD_bitset falses;

    if (context == MR_ROBDD_zero)
        return MR_ROBDD_zero;
    if (context == MR_ROBDD_one)
        return MR_ROBDD_restricted_rename(f, count, mapping, thresh);

    MR_ROBDD_BITSET_CLEAR(trues);
    MR_ROBDD_BITSET_CLEAR(falses);

    return MR_ROBDD_abexit(context, f, count, mapping, thresh,
        &trues, &falses, -1);
}

#endif /* 0 (end of unused code)  */

/****************************************************************

             Abstract Unification

 ****************************************************************/

/*
** NB:  MR_ROBDD_iff_conj_array, and so all the functions that call it, now
** allow v0 to apear in the array of variables arr[].  This makes the
** analyzer robust for handling goals like X = f(X, Y).  Since it's
** cheaper and easier to check this in C, I do it here.
*/

#if defined(MR_ROBDD_ELIM_DUPS)
  #define MR_ROBDD_DECLARE_PREV(init) MR_ROBDD_int prev = (init);
  #define MR_ROBDD_HANDLE_DUP(this, rel)                                \
    if ((this) == prev) continue;                                       \
    assert((this) rel prev);                                            \
    prev = (this);
#elif !defined(NDEBUG)
  #define MR_ROBDD_DECLARE_PREV(init) MR_ROBDD_int prev = (init);
  #define MR_ROBDD_HANDLE_DUP(this, rel)                                \
    assert((this) != prev);                                             \
    assert((this) rel prev);                                            \
    prev = (this);
#else
  #define MR_ROBDD_DECLARE_PREV(init)
  #define MR_ROBDD_HANDLE_DUP(this, rel)
#endif

MR_ROBDD_DECLARE_FN_COUNT(iff_conj)

#if defined(MR_ROBDD_NAIVE)
MR_ROBDD_node *
MR_ROBDD_iff_conj_array(MR_ROBDD_int v0, MR_ROBDD_int n, MR_ROBDD_int arr[])
{
    MR_ROBDD_node *conj = MR_ROBDD_one;
    MR_ROBDD_node *v_rep = MR_ROBDD_variableRep(v0);
    MR_ROBDD_int i;
    MR_ROBDD_DECLARE_PREV(-1)

    MR_ROBDD_COUNT_FN(iff_conj);
    /* We construct the conjunction from the lowest var up to the highest.
     * This is a quadratic process, while the other way is linear.
     */ 
    for (i=0; i<n; ++i) {
        MR_ROBDD_HANDLE_DUP(arr[i], >)
        if (arr[i] != v0) {
            conj = MR_ROBDD_glb(conj, MR_ROBDD_variableRep(arr[i]));
        }
    }
    return MR_ROBDD_glb(MR_ROBDD_implies(conj, v_rep),
        MR_ROBDD_implies(v_rep, conj));
}

#elif !defined(MR_ROBDD_USE_THRESH)

MR_ROBDD_node *
MR_ROBDD_iff_conj_array(MR_ROBDD_int v0, MR_ROBDD_int n, MR_ROBDD_int arr[])
{
    MR_ROBDD_node *conj = MR_ROBDD_one;
    MR_ROBDD_node *pos_v = MR_ROBDD_variableRep(v0);
    MR_ROBDD_node *neg_v = MR_ROBDD_ite(pos_v, MR_ROBDD_zero, MR_ROBDD_one);
    MR_ROBDD_int *ptr;
    MR_ROBDD_DECLARE_PREV(MR_ROBDD_MAXVAR)

    MR_ROBDD_COUNT_FN(iff_conj);
    /* Note that to be efficient, we construct the conjunction
     * from the highest var down to the lowest.  This is a linear
     * process, while the other way is n squared.
     */ 
    for (ptr=&arr[n-1]; ptr>=arr; --ptr) {
        MR_ROBDD_int vi = *ptr;
        MR_ROBDD_HANDLE_DUP(vi, <)
        if (vi != v0) {
            conj = MR_ROBDD_glb(conj, MR_ROBDD_variableRep(vi));
        }
    }
    return MR_ROBDD_ite(conj, pos_v, neg_v);
}

#else /* MR_ROBDD_USE_THRESH */

MR_ROBDD_node *
MR_ROBDD_iff_conj_array(MR_ROBDD_int v0, MR_ROBDD_int n, MR_ROBDD_int arr[])
{
    MR_ROBDD_node *thens = MR_ROBDD_one, *elses = MR_ROBDD_zero;
    MR_ROBDD_int *ptr;
    MR_ROBDD_int vi = 0;    /* this value doesn't matter */
    MR_ROBDD_DECLARE_PREV(MR_ROBDD_MAXVAR)

    MR_ROBDD_COUNT_FN(iff_conj);

    /*
    ** first build part of graph below v0. For this, we build two subgraphs:
    ** MR_ROBDD_one for when v0 is true, and MR_ROBDD_one for false.
    ** These are called thens and elses.
    */

    for (ptr=&arr[n-1]; ptr>=arr && v0<(vi=*ptr); --ptr) {
        MR_ROBDD_HANDLE_DUP(vi, <)
        thens = MR_ROBDD_make_node(vi, thens, MR_ROBDD_zero);
        elses = MR_ROBDD_make_node(vi, elses, MR_ROBDD_one);
    }

    if (v0 == vi) {
        --ptr;
    }

    /* make v0 MR_ROBDD_node */
    thens = MR_ROBDD_make_node(v0, thens, elses);

    /*
     * Finally build part of graph above v0.  For this, we build
     * only MR_ROBDD_one graph, whose then branch is the graph we've built
     * so far and whose else branch is ~v0.
     */

    if (ptr >= arr) {
        MR_ROBDD_DECLARE_PREV(MR_ROBDD_MAXVAR)
        /* make ~v0 */
        elses = MR_ROBDD_make_node(v0, MR_ROBDD_zero, MR_ROBDD_one);

        do {
            vi = *ptr;
            MR_ROBDD_HANDLE_DUP(vi, <)
            thens = MR_ROBDD_make_node(vi, thens, elses);
        } while (--ptr >= arr);
    }

    return thens;
}
#endif /* MR_ROBDD_OLD */

MR_ROBDD_node *
MR_ROBDD_restricted_iff_conj_array(MR_ROBDD_int v0, MR_ROBDD_int n,
    MR_ROBDD_int arr[], MR_ROBDD_int thresh)
{
    if (v0 > thresh) {
        return MR_ROBDD_one;
    }
    while (--n>=0 && arr[n]>thresh)
        ;
    return MR_ROBDD_iff_conj_array(v0, n+1, arr);
}

#if !defined(MR_ROBDD_USE_THRESH) && !defined(MR_ROBDD_RESTRICT_SET)
MR_ROBDD_node *
MR_ROBDD_abstract_unify(MR_ROBDD_node *f, MR_ROBDD_int v0,
    MR_ROBDD_int n, MR_ROBDD_int arr[], MR_ROBDD_int lo, MR_ROBDD_int hi)
{
    return MR_ROBDD_restricted_glb(lo, hi, f,
        MR_ROBDD_iff_conj_array(v0, n, arr));
}

#elif 1 /* ( MR_ROBDD_USE_THRESH || MR_ROBDD_RESTRICT_SET ) */

MR_ROBDD_node *
MR_ROBDD_abstract_unify(MR_ROBDD_node *f, MR_ROBDD_int v0,
    MR_ROBDD_int n, MR_ROBDD_int arr[], MR_ROBDD_int thresh)
{
    return MR_ROBDD_restricted_glb(thresh, f,
        MR_ROBDD_iff_conj_array(v0, n, arr));
}

#else /* !1 (this code is unused) */

static MR_ROBDD_node *
MR_ROBDD_build_and(MR_ROBDD_int n, MR_ROBDD_int arr[], MR_ROBDD_node *tr,
    MR_ROBDD_node *fa)
{
    if (n<=0) {
        return tr;
    } else {
        return MR_ROBDD_make_node(arr[0],
                 MR_ROBDD_build_and(n-1, &arr[1], tr, fa), fa);
    }
}

static MR_ROBDD_node *
MR_ROBDD_glb_and(MR_ROBDD_node *f, MR_ROBDD_int n, MR_ROBDD_int arr[])
{
    if (f == MR_ROBDD_zero) {
        return MR_ROBDD_zero;
    } else if (f == MR_ROBDD_one) {
        return MR_ROBDD_build_and(n, arr, MR_ROBDD_one, MR_ROBDD_zero);
    } else if (n == 0) {
        return f;
    } else {
        MR_ROBDD_node *result;
        if (f->value < arr[0]) {
            result = MR_ROBDD_make_node(f->value,
                   MR_ROBDD_glb_and(f->tr, n, arr),
                   MR_ROBDD_glb_and(f->fa, n, arr));
        } else if (f->value > arr[0]) {
            result = MR_ROBDD_make_node(arr[0],
                   MR_ROBDD_glb_and(f, n-1, &arr[1]),
                   MR_ROBDD_zero);
        } else /* f->value == arr[0] */{
            result = MR_ROBDD_make_node(f->value,
                   MR_ROBDD_glb_and(f->tr, n-1, &arr[1]),
                   MR_ROBDD_zero);
        }
        return result;
    }
}

static MR_ROBDD_node *
MR_ROBDD_glb_nand(MR_ROBDD_node *f, MR_ROBDD_int n, MR_ROBDD_int arr[])
{
    if (f == MR_ROBDD_zero) {
        return MR_ROBDD_zero;
    } else if (f == MR_ROBDD_one) {
        return MR_ROBDD_build_and(n, arr, MR_ROBDD_zero, MR_ROBDD_one);
    } else if (n == 0) {
        return MR_ROBDD_zero;
    } else {
        MR_ROBDD_node *result;
        if (f->value < arr[0]) {
            result = MR_ROBDD_make_node(f->value,
                   MR_ROBDD_glb_nand(f->tr, n, arr),
                   MR_ROBDD_glb_nand(f->fa, n, arr));
        } else if (f->value > arr[0]) {
            result = MR_ROBDD_make_node(arr[0],
                   MR_ROBDD_glb_nand(f, n-1, &arr[1]),
                   f);
        } else /* f->value == arr[0] */{
            result = MR_ROBDD_make_node(f->value,
                   MR_ROBDD_glb_nand(f->tr, n-1, &arr[1]),
                   f);
        }
        return result;
    }
}

/*
** returns MR_ROBDD_zero != MR_ROBDD_glb(f,
**  MR_ROBDD_build_and(n, arr, MR_ROBDD_one, MR_ROBDD_zero))
*/

static MR_ROBDD_int
MR_ROBDD_exists_glb_and(MR_ROBDD_node *f, MR_ROBDD_int n, MR_ROBDD_int arr[])
{
    if (f == MR_ROBDD_zero) {
        return MR_FALSE;
    } else if (f == MR_ROBDD_one || n == 0) {
        return MR_TRUE;
    } else if (f->value < arr[0]) {
        return (MR_ROBDD_exists_glb_and(f->tr, n, arr) ||
            MR_ROBDD_exists_glb_and(f->fa, n, arr));
    } else {
        if (f->value == arr[0]) {
            f = f->tr;
        }
        return MR_ROBDD_exists_glb_and(f, n-1, arr+1);
    }
}

/*
** returns MR_ROBDD_zero != MR_ROBDD_glb(f,
**  MR_ROBDD_build_and(n, arr, MR_ROBDD_zero, MR_ROBDD_one))
*/

static MR_ROBDD_int
MR_ROBDD_exists_glb_nand(MR_ROBDD_node *f, MR_ROBDD_int n, MR_ROBDD_int arr[])
{
    if (f == MR_ROBDD_zero || n == 0) {
        return MR_FALSE;
    } else if (f == MR_ROBDD_one || f->value > arr[0]) {
        /*
        ** if f->value > arr[0], then the else branch of the MR_ROBDD_node we'd
        ** build would be MR_ROBDD_one, so we know the graph couldn't ==
        ** MR_ROBDD_zero.
        */
        return MR_TRUE;
    } else if (f->value < arr[0]) {
        return MR_ROBDD_glb_nand(f->tr, n, arr)
            || MR_ROBDD_glb_nand(f->fa, n, arr);
    } else if (f->fa != MR_ROBDD_zero) /* && f->value == arr[0] */ {
        /*
        ** if f->fa isn't MR_ROBDD_zero, then conjoined with MR_ROBDD_one
        ** it won't be MR_ROBDD_zero, so the MR_ROBDD_node we'd build won't
        ** == MR_ROBDD_zero.
        */
        return MR_TRUE;
    } else /* f->value == arr[0] && f->fa == MR_ROBDD_zero */ {
        return MR_ROBDD_exists_glb_nand(f->tr, n-1, arr+1);
    }
}

/*
** returns MR_ROBDD_zero != MR_ROBDD_glb(f,
**  MR_ROBDD_iff_conj_array(v0, n, arr))
*/

static MR_ROBDD_int
MR_ROBDD_exists_glb_iffconj(MR_ROBDD_node *f, MR_ROBDD_int v0, MR_ROBDD_int n,
    MR_ROBDD_intarr[])
{
    if (f == MR_ROBDD_zero) {
        return MR_FALSE;
    } else if (f == MR_ROBDD_one) {
        return MR_TRUE;
    } else {
        MR_ROBDD_int v = (n>0 && arr[0]<v0) ? arr[0] : v0;
        
        if (f->value < v) {
            return (MR_ROBDD_exists_glb_iffconj(f->tr, v0, n, arr) ||
                MR_ROBDD_exists_glb_iffconj(f->fa, v0, n, arr));
        } else /* f->value >= v */ {
            MR_ROBDD_node *tr, *fa;

            if (f->value == v) {
                tr = f->tr; fa = f->fa;
            } else /* f->value > v */ {
                tr = f; fa = f;
            }

            if (v == v0) {
                return (MR_ROBDD_exists_glb_nand(fa, n, arr) ||
                    MR_ROBDD_exists_glb_and(tr, n, arr));
            } else {
                return (!MR_ROBDD_var_entailed(fa, v0) ||
                    MR_ROBDD_exists_glb_iffconj(tr, v0, n-1, &arr[1]));
            }
        }
    }
}

/*
** returns MR_ROBDD_restricted_glb(f,
**  MR_ROBDD_build_and(n, arr, MR_ROBDD_one, MR_ROBDD_zero), thresh)
*/

static MR_ROBDD_node *
MR_ROBDD_rglb_and(MR_ROBDD_node *f, MR_ROBDD_int n, MR_ROBDD_int arr[],
    MR_ROBDD_int thresh)
{
    if (f == MR_ROBDD_zero) {
        return MR_ROBDD_zero;
    } else if (f == MR_ROBDD_one) {
        while (--n>=0 && arr[n]>thresh);
        return MR_ROBDD_build_and(n+1, arr, MR_ROBDD_one, MR_ROBDD_zero);
    } else if (n == 0) {
        return MR_ROBDD_restrictThresh(thresh, f);
    } else {
        if (f->value < arr[0]) {
            if (f->value > thresh) {
                return (MR_ROBDD_exists_glb_and(f->tr, n, arr) ||
                        MR_ROBDD_exists_glb_and(f->fa, n, arr))
                            ? MR_ROBDD_one : MR_ROBDD_zero;
            } else {
                return MR_ROBDD_make_node(f->value,
                           MR_ROBDD_rglb_and(f->tr, n, arr, thresh),
                           MR_ROBDD_rglb_and(f->fa, n, arr, thresh));
            }
        } else /* arr[0] <= f->value */ {
            MR_ROBDD_int v = arr[0];

            if (v == f->value) {
                f = f->tr;
            }
            if (v > thresh) {
                return MR_ROBDD_exists_glb_and(f, n-1, arr+1)
                    ? MR_ROBDD_one : MR_ROBDD_zero;
            } else {
                return MR_ROBDD_make_node(v,
                         MR_ROBDD_rglb_and(f, n-1, arr+1, thresh),
                         MR_ROBDD_zero);
            }
        }
    }
}

/*
** returns MR_ROBDD_restricted_glb(f,
**  MR_ROBDD_build_and(n, arr, MR_ROBDD_zero, MR_ROBDD_one), thresh)
*/

static MR_ROBDD_node *
MR_ROBDD_rglb_nand(MR_ROBDD_node *f, MR_ROBDD_int n, MR_ROBDD_int arr[],
    MR_ROBDD_int thresh)
{
    if (f == MR_ROBDD_zero || n == 0) {
        return MR_ROBDD_zero;
    } else if (f == MR_ROBDD_one) {
        /*
        ** just MR_ROBDD_restrictThresh(thresh,
        ** MR_ROBDD_build_and(n, arr, MR_ROBDD_zero, MR_ROBDD_one)).
        ** Note that if anything is restricted away, then the whole graph
        ** degenerates to MR_ROBDD_one.  So...
        */
        if (arr[n-1]>thresh) {
            return MR_ROBDD_one;
        } else {
            return MR_ROBDD_build_and(n, arr, MR_ROBDD_zero, MR_ROBDD_one);
        }
    } else {
        if (f->value < arr[0]) {
            if (f->value > thresh) {
                return (MR_ROBDD_exists_glb_nand(f->tr, n, arr)||
                        MR_ROBDD_exists_glb_nand(f->fa, n, arr))
                            ? MR_ROBDD_one : MR_ROBDD_zero;
            } else {
                return MR_ROBDD_make_node(f->value,
                         MR_ROBDD_rglb_nand(f->tr, n, arr, thresh),
                         MR_ROBDD_rglb_nand(f->fa, n, arr, thresh));
            }
        } else if (f->value > arr[0]) {
            if (arr[0] > thresh) {
                /* because f != MR_ROBDD_zero, restricting all vars yields MR_ROBDD_one */
                return MR_ROBDD_one;
            } else {
                return MR_ROBDD_make_node(arr[0],
                         MR_ROBDD_rglb_nand(f, n-1, arr+1, thresh),
                         MR_ROBDD_restrictThresh(thresh, f));
            }
        } else /* f->value == arr[0] */ {
            if (arr[0] > thresh) {
                return (f->fa != MR_ROBDD_zero ||
                    MR_ROBDD_exists_glb_nand(f->tr, n-1, arr+1))
                        ? MR_ROBDD_one : MR_ROBDD_zero;
            } else {
                return MR_ROBDD_make_node(arr[0],
                         MR_ROBDD_rglb_nand(f->tr, n-1, arr+1, thresh),
                         MR_ROBDD_restrictThresh(thresh, f->fa));
            }
        }
    }
}

/*
** returns MR_ROBDD_restricted_glb(thresh, f,
**  MR_ROBDD_make_node(v, MR_ROBDD_zero, MR_ROBDD_one))
*/

static MR_ROBDD_node *
MR_ROBDD_rglb_notvar(MR_ROBDD_node *f, MR_ROBDD_int v, MR_ROBDD_int thresh)
{
    if (f == MR_ROBDD_zero) {
        return MR_ROBDD_zero;
    } else if (f == MR_ROBDD_one) {
        if (v > thresh) return MR_ROBDD_one;
        return MR_ROBDD_make_node(v, MR_ROBDD_zero, MR_ROBDD_one);
    } else {
        if (f->value < v) {
            if (f->value > thresh) {
                return (!MR_ROBDD_var_entailed(f->tr, v) ||
                       !MR_ROBDD_var_entailed(f->fa, v))
                            ? MR_ROBDD_one : MR_ROBDD_zero;
            } else {
                return MR_ROBDD_make_node(f->value,
                         MR_ROBDD_rglb_notvar(f->tr, v, thresh),
                         MR_ROBDD_rglb_notvar(f->fa, v, thresh));
            }
        } else {
            if (f->value == v) {
                f = f->fa;
            }
            if (v > thresh) {
                return f==MR_ROBDD_zero ? MR_ROBDD_zero : MR_ROBDD_one;
            } else {
                return MR_ROBDD_make_node(v, MR_ROBDD_zero,
                    MR_ROBDD_restrictThresh(thresh, f));
            }
        }
    }
}

MR_ROBDD_node *
MR_ROBDD_abstract_unify(MR_ROBDD_node *f, MR_ROBDD_int v0, MR_ROBDD_int n,
    MR_ROBDD_int arr[], MR_ROBDD_int thresh)
{
    if (f == MR_ROBDD_zero) {
        return MR_ROBDD_zero;
    } else if (f == MR_ROBDD_one) {
        return MR_ROBDD_restricted_iff_conj_array(v0, n, arr, thresh);
    } else {
        MR_ROBDD_int v = (n>0 && arr[0]<v0) ? arr[0] : v0;
        MR_ROBDD_node *result;
        
        if (f->value < v) {
            if (f->value > thresh) {
                result = (MR_ROBDD_exists_glb_iffconj(f->tr, v0, n, arr) ||
                        MR_ROBDD_exists_glb_iffconj(f->fa, v0, n, arr))
                            ? MR_ROBDD_one : MR_ROBDD_zero;
            } else {
                result =
                  MR_ROBDD_make_node(f->value,
                    MR_ROBDD_abstract_unify(f->tr, v0, n, arr, thresh),
                    MR_ROBDD_abstract_unify(f->fa, v0, n, arr, thresh));
            }
        } else /* f->value >= v */ {
            MR_ROBDD_node *tr = f, *fa = f;

            if (f->value == v) {
                tr = f->tr; fa = f->fa;
            }
            if (v > thresh) {
                MR_ROBDD_int value;

                if (v == v0) {
                    value = (MR_ROBDD_exists_glb_nand(fa, n, arr) ||
                         MR_ROBDD_exists_glb_and(tr, n, arr));
                } else {
                    value =
                      (!MR_ROBDD_var_entailed(fa, v0) ||
                       MR_ROBDD_exists_glb_iffconj(tr, v0, n-1, &arr[1]));
                }
                result = value ? MR_ROBDD_one : MR_ROBDD_zero;
            } else {
                if (v == v0) {
                    tr = MR_ROBDD_rglb_and(tr, n, arr, thresh);
                    fa = MR_ROBDD_rglb_nand(fa, n, arr, thresh);
                } else {
                    tr = MR_ROBDD_abstract_unify(tr, v0, n-1, arr+1, thresh);
                    fa = MR_ROBDD_rglb_notvar(fa, v0, thresh);
                }
                result = MR_ROBDD_make_node(v, tr, fa);
            }
        }
        return result;
    }
}

#endif /* (end of unused code) */

/****************************************************************

              Finding Entailed Variables

 ****************************************************************/

/*
** returns MR_TRUE iff var is implied by the boolean function specified by f
*/

int
MR_ROBDD_var_entailed(MR_ROBDD_node *f, MR_ROBDD_int var)
{
#if defined(MR_ROBDD_NAIVE)
    return f == MR_ROBDD_glb(f, MR_ROBDD_variableRep(var));
#elif !defined(MR_ROBDD_USE_THRESH) && !defined(MR_ROBDD_USE_ITE_CONSTANT)
    return MR_ROBDD_one == MR_ROBDD_implies(f, MR_ROBDD_variableRep(var));
#elif !defined(MR_ROBDD_USE_RGLB)
    return MR_ROBDD_one == MR_ROBDD_ite_constant(f, MR_ROBDD_variableRep(var),
        MR_ROBDD_one);
#else /* MR_ROBDD_USE_RGLB */
    if (MR_ROBDD_IS_TERMINAL(f)) {
        return f==MR_ROBDD_zero;
    } else {
        MR_ROBDD_DECLARE_VAR_ENTAILED_CACHE_ENTRY
        int result;

        MR_ROBDD_TRY_VAR_ENTAILED_CACHE(f, var);
        if (f->value < var) {
            result = MR_ROBDD_var_entailed(f->tr, var)
                  && MR_ROBDD_var_entailed(f->fa, var);
        } else if (f->value == var) {
            result = (f->fa==MR_ROBDD_zero);
        } else /* f->value > var */ {
            result = MR_FALSE;
        }
        MR_ROBDD_UPDATE_VAR_ENTAILED_CACHE(f, var, result);
        return result;
    }
#endif /* MR_ROBDD_NEW */
}

/*
** returns a MR_ROBDD_bitset of all the variables implied by f.  A variable i
** is implied by f iff
**
**    MR_ROBDD_BITSET_IS_MEMBER(*result, i)
*/

#if !defined(MR_ROBDD_NEW)
MR_ROBDD_bitset *
MR_ROBDD_vars_entailed(MR_ROBDD_node *f, MR_ROBDD_int MR_ROBDD_topvar)
{
    static MR_ROBDD_bitset def_vars;
    MR_ROBDD_int i;

    MR_ROBDD_BITSET_CLEAR(def_vars);

    for (i=0; i<MR_ROBDD_topvar; ++i) {
        if (MR_ROBDD_var_entailed(f, i)) {
            MR_ROBDD_BITSET_ADD_ELEMENT(def_vars, i);
        }
    }
    return &def_vars;
}

#elif 0    /* not using this version */

MR_ROBDD_int
MR_ROBDD_topvar(MR_ROBDD_node *f, MR_ROBDD_int n)
{
    MR_ROBDD_int n1;

    if (MR_ROBDD_IS_TERMINAL(f)) return n;

    n1 = MR_ROBDD_topvar(f->tr, f->value);
    if (n1 > n) {
        n = n1;
    }
    return MR_ROBDD_topvar(f->fa, n);
}

MR_ROBDD_bitset *
MR_ROBDD_vars_entailed(MR_ROBDD_node *f)
{
    static MR_ROBDD_bitset def_vars;
    MR_ROBDD_int i = MR_ROBDD_topvar(f, 0);

    MR_ROBDD_BITSET_CLEAR(def_vars);

    for (; i >= 0; --i) {
        if (MR_ROBDD_var_entailed(f, i)) {
            MR_ROBDD_BITSET_ADD_ELEMENT(def_vars, i);
        }
    }
    return &def_vars;
}

#else /* MR_ROBDD_NEW */

/*
** According to Roberto Bagnara's benchmarking, this version is slower
** than the simpler version below.
*/

#if defined(OLD_VARS_ENTAILED)

/*
** these variables should be local to both MR_ROBDD_vars_entailed and
** MR_ROBDD_vars_entailed_aux, but that isn't possible in C, so I have to make
** them global.  They should not be use by any other functions.
*/

static MR_ROBDD_bitset MR_ROBDD_this_path;
static MR_ROBDD_bitset MR_ROBDD_entailed;

void
MR_ROBDD_vars_entailed_aux(MR_ROBDD_node *f)
{
    while (!MR_ROBDD_IS_TERMINAL(f)) {
        MR_ROBDD_int word = MR_ROBDD_BITSET_WORD(f->value);
        MR_ROBDD_bitmask mask = MR_ROBDD_BITSET_MASK(f->value);
        
        /* turn on bit for then branch */
        MR_ROBDD_BITSET_ADD(MR_ROBDD_this_path, word, mask);
        MR_ROBDD_vars_entailed_aux(f->tr);
        /* turn it off for the else branch */
        MR_ROBDD_BITSET_TOGGLE(MR_ROBDD_this_path, word, mask);
        f = f->fa;
    }
    /* needn't do anything for false terminals */
    if (f == MR_ROBDD_one) {
        MR_ROBDD_BITSET_INTERSECTION(MR_ROBDD_entailed, MR_ROBDD_entailed,
            MR_ROBDD_this_path);
    }
}

/*
** returns a MR_ROBDD_bitset of all the variables implied by f.  A variable i
** is implied by f iff
**
**    MR_ROBDD_BITSET_IS_MEMBER(*result, i)
*/

MR_ROBDD_bitset *
MR_ROBDD_vars_entailed(MR_ROBDD_node *f)
{
    MR_ROBDD_BITSET_CLEAR(MR_ROBDD_this_path);
    MR_ROBDD_BITSET_UNIVERSE(MR_ROBDD_entailed);

    MR_ROBDD_vars_entailed_aux(f);
    return &MR_ROBDD_entailed;
}

#else /* MR_ROBDD_NEW && !OLD_VARS_ENTAILED */

/*
** returns a MR_ROBDD_bitset of all the variables implied by f.  A variable i
** is implied by f iff
**
**    MR_ROBDD_BITSET_IS_MEMBER(*result, i)
*/

MR_ROBDD_bitset *
MR_ROBDD_vars_entailed(MR_ROBDD_node *f)
{
    static MR_ROBDD_bitset tmp_bitset;
    MR_ROBDD_DECLARE_UNARY_BITSET_CACHE_ENTRY

    if (f == MR_ROBDD_zero) {
        MR_ROBDD_BITSET_UNIVERSE(tmp_bitset);
    } else if (f == MR_ROBDD_one) {
        MR_ROBDD_BITSET_CLEAR(tmp_bitset);
    } else {
        MR_ROBDD_bitset bs;

        MR_ROBDD_TRY_UNARY_BITSET_CACHE(f, MR_ROBDD_vars_entailed);
        tmp_bitset = *MR_ROBDD_vars_entailed(f->tr);
        bs = *MR_ROBDD_vars_entailed(f->fa);
        MR_ROBDD_BITSET_INTERSECTION(tmp_bitset, tmp_bitset, bs);
        if (f->fa == MR_ROBDD_zero) {
            MR_ROBDD_BITSET_ADD_ELEMENT(tmp_bitset, f->value);
        }
        MR_ROBDD_UPDATE_UNARY_BITSET_CACHE(f, tmp_bitset,
            MR_ROBDD_vars_entailed);
    }
    return &tmp_bitset;
}

#endif /* MR_ROBDD_NEW && !OLD_VARS_ENTAILED */
#endif /* MR_ROBDD_NEW */

/****************************************************************

            Set Sharing Operations

 ****************************************************************/
#if defined(MR_ROBDD_SHARING)

/*
** returns the Boolean function
**    ~1&~2&...~n | 1&~2&...~n | ~1&2&...~n | ~1&~2&...n
*/

MR_ROBDD_node *
MR_ROBDD_init_set_sharing(MR_ROBDD_int n)
{
    MR_ROBDD_node *result = MR_ROBDD_one;
    MR_ROBDD_node *other = MR_ROBDD_one;

    while (n>1) {
        other = MR_ROBDD_make_node(n, MR_ROBDD_zero, other);
        result = MR_ROBDD_make_node(--n, other, result);
    }

    return result;
}

#if defined(MR_ROBDD_NEW)

MR_ROBDD_DECLARE_FN_COUNT(MR_ROBDD_complete_one_or)

/* the same as MR_ROBDD_lub(MR_ROBDD_complete_one(f), prev) */
MR_ROBDD_node *
MR_ROBDD_complete_one_or(MR_ROBDD_node *f, MR_ROBDD_node *prev)
{
    MR_ROBDD_node *result;
    MR_ROBDD_node *MR_ROBDD_complete_one(MR_ROBDD_node *f);
    MR_ROBDD_DECLARE_ASYM_BIN_CACHE_ENTRY

    MR_ROBDD_COUNT_FN(MR_ROBDD_complete_one_or);

    if (MR_ROBDD_IS_TERMINAL(prev)) {
        return (prev==MR_ROBDD_one) ? MR_ROBDD_one : MR_ROBDD_complete_one(f);
    }
    if (MR_ROBDD_IS_TERMINAL(f)) {
        return (f==MR_ROBDD_one) ? MR_ROBDD_one : prev;
    }

    MR_ROBDD_TRY_ASYM_BIN_CACHE(f, prev, MR_ROBDD_complete_one_or);

    /*
    ** we want to return:
    **     MR_ROBDD_lub(MR_ROBDD_make_node(f->value,
    **          MR_ROBDD_lub(MR_ROBDD_complete_one(f->tr),
    **                  MR_ROBDD_complete_one(f->fa)),
    **                  MR_ROBDD_complete_one(f->fa)),
    **          prev);
    ** but we want to unfold the outer MR_ROBDD_lub call, so we compare
    ** prev->value and f->value.  Actually, this isn't right when
    ** f->value <= prev->value, because the MR_ROBDD_make_node call may return
    ** a MR_ROBDD_node whose label is > prev->value, but in that case,
    **    MR_ROBDD_lub(that, prev) = MR_ROBDD_make_node(prev->value,
    **              MR_ROBDD_lub(that, prev->tr), MR_ROBDD_lub(that, prev->fa))
    */

    if (f->value < prev->value) {
        MR_ROBDD_node *cfa = MR_ROBDD_complete_one_or(f->fa, prev);
        result = MR_ROBDD_make_node(f->value,
            MR_ROBDD_complete_one_or(f->tr, cfa), cfa);
    } else if (f->value == prev->value) {
        result = MR_ROBDD_make_node(f->value,
                   MR_ROBDD_complete_one_or(f->tr,
                       MR_ROBDD_complete_one_or(f->fa,
                           prev->tr)),
                   MR_ROBDD_complete_one_or(f->fa, prev->fa));
    } else {
        result = MR_ROBDD_make_node(prev->value,
                   MR_ROBDD_complete_one_or(f, prev->tr),
                   MR_ROBDD_complete_one_or(f, prev->fa));
    }
    MR_ROBDD_UPDATE_ASYM_BIN_CACHE(f, prev, result, MR_ROBDD_complete_one_or);
    return result;
}

MR_ROBDD_DECLARE_FN_COUNT(MR_ROBDD_complete_one)

MR_ROBDD_node *
MR_ROBDD_complete_one(MR_ROBDD_node *f)
{
    MR_ROBDD_node *result, *cfa;
    MR_ROBDD_DECLARE_UNARY_CACHE_ENTRY

    MR_ROBDD_COUNT_FN(MR_ROBDD_complete_one);

    if (MR_ROBDD_IS_TERMINAL(f)) {
        return f;
    }
    MR_ROBDD_TRY_UNARY_CACHE(f, MR_ROBDD_complete_one);
    cfa = MR_ROBDD_complete_one(f->fa);
    result = MR_ROBDD_make_node(f->value,
        MR_ROBDD_complete_one_or(f->tr, cfa), cfa);
    MR_ROBDD_UPDATE_UNARY_CACHE(f, result, MR_ROBDD_complete_one);
    return result;
}

MR_ROBDD_DECLARE_FN_COUNT(MR_ROBDD_complete_or)

/* the same as MR_ROBDD_lub(MR_ROBDD_complete(f, g), prev) */
MR_ROBDD_node *
MR_ROBDD_complete_or(MR_ROBDD_node *f, MR_ROBDD_node *g, MR_ROBDD_node *prev)
{
    MR_ROBDD_node *result;
    MR_ROBDD_node *lo, *hi;
    MR_ROBDD_DECLARE_COMPLETE_OR_CACHE_ENTRY

    MR_ROBDD_COUNT_FN(MR_ROBDD_complete_or);

    if (prev == MR_ROBDD_one) {
        return MR_ROBDD_one;
    }
    if (f == g) {
        return MR_ROBDD_lub(f, prev);
    }
    if (f == MR_ROBDD_zero || g == MR_ROBDD_zero) {
        return prev;
    }

    if (f == MR_ROBDD_one) {
        return MR_ROBDD_complete_one_or(g, prev);
    }
    if (g == MR_ROBDD_one) {
        return MR_ROBDD_complete_one_or(f, prev);
    }
    if (prev == MR_ROBDD_zero) {
        return MR_ROBDD_complete(f, g);
    }

    MR_ROBDD_TRY_COMPLETE_OR_CACHE(f, g, prev);

    if (f->value > g->value) {
        lo=g;
        hi=f;
    } else {
        lo=f;
        hi=g;
    }

    if (prev->value < lo->value) {
        result = MR_ROBDD_make_node(prev->value,
                   MR_ROBDD_complete_or(lo, hi, prev->tr),
                   MR_ROBDD_complete_or(lo, hi, prev->fa));
    } else if (prev->value == lo->value) {
        if (lo->value == hi->value) {
            MR_ROBDD_node *n = MR_ROBDD_complete_or(lo->tr, hi->tr, prev->tr);
            n = MR_ROBDD_complete_or(lo->tr, hi->fa, n);
            n = MR_ROBDD_complete_or(lo->fa, hi->tr, n);
            result = MR_ROBDD_make_node(lo->value, n,
                       MR_ROBDD_complete_or(lo->fa, hi->fa, prev->fa));
        } else {
            MR_ROBDD_node *n = MR_ROBDD_complete_or(lo->fa, hi, prev->tr);
            n = MR_ROBDD_complete_or(lo->tr, hi, n);
            result = MR_ROBDD_make_node(lo->value, n,
                       MR_ROBDD_complete_or(lo->fa, hi, prev->fa));
        }
    } else if (lo->value == hi->value) {
        MR_ROBDD_node *n = MR_ROBDD_complete_or(lo->tr, hi->tr, prev);
        n = MR_ROBDD_complete_or(lo->tr, hi->fa, n);
        n = MR_ROBDD_complete_or(lo->fa, hi->tr, n);
        result = MR_ROBDD_make_node(lo->value, n,
                   MR_ROBDD_complete_or(lo->fa, hi->fa, prev));
    } else {
        MR_ROBDD_node *n = MR_ROBDD_complete_or(lo->fa, hi, prev);
        result = MR_ROBDD_make_node(lo->value,
            MR_ROBDD_complete_or(lo->tr, hi, n), n);
    }
    MR_ROBDD_UPDATE_COMPLETE_OR_CACHE(f, g, prev, result);

    return result;
}

#else /* !MR_ROBDD_NEW */

#define MR_ROBDD_complete_or(x, y, z) \
    MR_ROBDD_lub(MR_ROBDD_complete((x),(y)),(z))
#endif

MR_ROBDD_DECLARE_FN_COUNT(MR_ROBDD_complete)

MR_ROBDD_node *
MR_ROBDD_complete(MR_ROBDD_node *f, MR_ROBDD_node *g)
{
    MR_ROBDD_node *result;
    MR_ROBDD_int fvar, gvar;
    MR_ROBDD_DECLARE_BIN_CACHE_ENTRY

    MR_ROBDD_COUNT_FN(MR_ROBDD_complete);

    if (f == g) {
        return f;
    }
    if (f == MR_ROBDD_zero || g == MR_ROBDD_zero) {
        return MR_ROBDD_zero;
    }

    MR_ROBDD_TRY_BIN_CACHE(f, g, MR_ROBDD_complete);

    fvar = (f == MR_ROBDD_one) ? MR_ROBDD_MAXVAR : f->value;
    gvar = (g == MR_ROBDD_one) ? MR_ROBDD_MAXVAR : g->value;
    if (fvar == gvar) {
        result = MR_ROBDD_make_node(fvar,
             MR_ROBDD_complete_or(f->fa, g->tr,
                 MR_ROBDD_complete_or(f->tr, g->tr,
                 MR_ROBDD_complete(f->tr, g->fa))),
             MR_ROBDD_complete(f->fa, g->fa));
    } else if (fvar < gvar) {
        MR_ROBDD_node *cfa = MR_ROBDD_complete(f->fa, g);

        result = MR_ROBDD_make_node(fvar, MR_ROBDD_complete_or(f->tr, g, cfa),
            cfa);
    } else /* fvar > gvar */ {
        MR_ROBDD_node *cfa = MR_ROBDD_complete(f, g->fa);

        result = MR_ROBDD_make_node(gvar, MR_ROBDD_complete_or(f, g->tr, cfa),
            cfa);
    }
    MR_ROBDD_UPDATE_BIN_CACHE(f, g, result, MR_ROBDD_complete);
    return result;
}

MR_ROBDD_DECLARE_FN_COUNT(MR_ROBDD_upclose)

MR_ROBDD_node *
MR_ROBDD_upclose(MR_ROBDD_node *f)
{
    MR_ROBDD_node *utr, *ufa, *result;
    MR_ROBDD_DECLARE_UNARY_CACHE_ENTRY

    MR_ROBDD_COUNT_FN(MR_ROBDD_upclose);

    if (MR_ROBDD_IS_TERMINAL(f)) {
        return f;
    }
    MR_ROBDD_TRY_UNARY_CACHE(f, MR_ROBDD_upclose);
    utr = MR_ROBDD_upclose(f->tr);
    ufa = MR_ROBDD_upclose(f->fa);
    result = MR_ROBDD_make_node(f->value, MR_ROBDD_complete_or(utr, ufa, utr),
        ufa);
    MR_ROBDD_UPDATE_UNARY_CACHE(f, result, MR_ROBDD_upclose);
    return result;
}

/*
** MR_ROBDD_bin(f, g)
**
** We note that the set of Boolean functions of n variables is
** isomorphic to the powerset of the set of n variables.  Thus we can
** use a Boolean function to represent a subset of the powerset.  We
** choose to let the *absence* of a variable from the set be indicated
** by that variable being true in the boolean function.
**
** This function computes the set of all possible unions of sets from
** f and g.
*/

MR_ROBDD_node *
MR_ROBDD_bin(MR_ROBDD_node *f, MR_ROBDD_node *g)
{
    if (f == MR_ROBDD_zero || g == MR_ROBDD_zero) {
        return MR_ROBDD_zero;
    } else if (f == MR_ROBDD_one) {
        return MR_ROBDD_bin_univ(g);
    } else if (g == MR_ROBDD_one) {
        return MR_ROBDD_bin_univ(f);
    } else {
        MR_ROBDD_node *result;
        MR_ROBDD_DECLARE_BIN_CACHE_ENTRY

        MR_ROBDD_TRY_BIN_CACHE(f, g, MR_ROBDD_bin);

        if (f->value == g->value) {
            MR_ROBDD_node *th = MR_ROBDD_bin(f->tr, g->tr);
            MR_ROBDD_node *el = MR_ROBDD_bin(f->tr, g->fa);

            if (el != MR_ROBDD_one) {
                el = MR_ROBDD_lub(el, MR_ROBDD_bin(f->fa, g->tr));
                if (el != MR_ROBDD_one) {
                    el = MR_ROBDD_lub(el, MR_ROBDD_bin(f->fa, g->fa));
                }
            }
            result = MR_ROBDD_make_node(f->value, th, el);
        } else {
            MR_ROBDD_node *tmp;

            if (f->value > g->value) {
                tmp = f; f = g; g = tmp;
            }
            /* now f->value < g->value */
            tmp = MR_ROBDD_bin(f->tr, g);
            if (tmp == MR_ROBDD_one)
                return MR_ROBDD_one;
            result = MR_ROBDD_make_node(f->value, tmp,
                MR_ROBDD_lub(tmp, MR_ROBDD_bin(f->fa, g)));
        }
        MR_ROBDD_UPDATE_BIN_CACHE(f, g, result, MR_ROBDD_bin);
        return result;
    }
}

/*
** Auxilliary function for MR_ROBDD_bin: special case code for
** MR_ROBDD_bin(f, MR_ROBDD_one).
*/

MR_ROBDD_node *
MR_ROBDD_bin_univ(MR_ROBDD_node *f)
{
    MR_ROBDD_node *g;

    if (MR_ROBDD_IS_TERMINAL(f)) {
        return f;
    }
    g = MR_ROBDD_bin_univ(f->tr);
    if (g == MR_ROBDD_one) {
        return MR_ROBDD_one;
    }
    return MR_ROBDD_make_node(f->value, g,
        MR_ROBDD_lub(g, MR_ROBDD_bin_univ(f->fa)));
}

#endif /* MR_ROBDD_SHARING */

/****************************************************************

              Initialization and Cleanup

 ****************************************************************/

#if defined(MR_ROBDD_STATISTICS)

void
MR_ROBDD_print_distribution(MR_ROBDD_int array[], MR_ROBDD_int MR_ROBDD_max)
{
    MR_ROBDD_int count;
    MR_ROBDD_int sum;
    MR_ROBDD_int total;
    MR_ROBDD_int zero_count;

    for (count=0, total=0; count<=MR_ROBDD_max; ++count) {
        total += array[count];
    }
    zero_count = array[0];
    for (count=0, sum=0; count<=MR_ROBDD_max; ++count) {
        if (array[count] > 0) {
            sum += array[count];
            printf(
                "%5ld nodes:%6ld %5.2f%% (cum = %6ld %5.1f%%, >0 = %6ld %5.1f%%)\n",
                (long) count, (long) array[count],
                MR_ROBDD_PERCENTAGE(array[count], total),
                (long) sum, MR_ROBDD_PERCENTAGE(sum, total),
                (long) (sum - zero_count),
                total == zero_count ? 999.999
                    : MR_ROBDD_PERCENTAGE(sum-zero_count, total-zero_count));
        }
    }
    if (array[MR_ROBDD_max+1] > 0) {
        printf(">%4ild nodes:%6ld %2.2f%%\n",
           (long) MR_ROBDD_max, (long) array[MR_ROBDD_max],
           MR_ROBDD_PERCENTAGE(array[MR_ROBDD_max], total));
    }
    printf("\n");
}
#endif /* MR_ROBDD_STATISTICS */

void
MR_ROBDD_initRep(void)
{
    MR_ROBDD_INIT_FN_COUNT(MR_ROBDD_make_node);
    MR_ROBDD_INIT_FN_COUNT(MR_ROBDD_variableRep);
    MR_ROBDD_INIT_FN_COUNT(MR_ROBDD_implies);
    MR_ROBDD_INIT_FN_COUNT(MR_ROBDD_lub);
    MR_ROBDD_INIT_FN_COUNT(MR_ROBDD_glb);
    MR_ROBDD_INIT_FN_COUNT(MR_ROBDD_restrict);
    MR_ROBDD_INIT_FN_COUNT(MR_ROBDD_restrictThresh);
    MR_ROBDD_INIT_FN_COUNT(MR_ROBDD_renameArray);
    MR_ROBDD_INIT_FN_COUNT(MR_ROBDD_reverseRenameArray);
    MR_ROBDD_INIT_FN_COUNT(MR_ROBDD_ite);
#if defined(MR_ROBDD_USE_ITE_CONSTANT)
    MR_ROBDD_INIT_FN_COUNT(MR_ROBDD_ite_constant);
#endif /* MR_ROBDD_USE_ITE_CONSTANT */
    MR_ROBDD_INIT_FN_COUNT(iff_conj);
#if defined(MR_ROBDD_NEW)
    MR_ROBDD_INIT_FN_COUNT(MR_ROBDD_ite_var);
    MR_ROBDD_INIT_FN_COUNT(MR_ROBDD_vars_entailed);
#endif
#if defined(MR_ROBDD_SHARING)
  #if defined(MR_ROBDD_NEW)
        MR_ROBDD_INIT_FN_COUNT(MR_ROBDD_complete_one_or);
        MR_ROBDD_INIT_FN_COUNT(MR_ROBDD_complete_one);
        MR_ROBDD_INIT_FN_COUNT(MR_ROBDD_complete_or);
  #endif /* MR_ROBDD_NEW */
        MR_ROBDD_INIT_FN_COUNT(MR_ROBDD_complete);
        MR_ROBDD_INIT_FN_COUNT(MR_ROBDD_upclose);
#endif /* MR_ROBDD_SHARING */
    /*
    ** This code clears the unique table and all the computed
    ** caches.  This should make it possible to time repeated
    ** execution of computations without the effect that after the
    ** first time the appropriate cache will probably hold the
    ** right result, so after the first time through the timing is
    ** useless.  Even if this isn't the case, after the first time
    ** performing a computation no new nodes will ever be created
    ** because they will already exist in the unique table.
    **
    ** After this, all old pointers to ROBDDs *must* be forgotten,
    ** because we free all allocated nodes, thus giving us a fresh
    ** start.
    */

    MR_ROBDD_INIT_UNIQUE_TABLE;

    MR_ROBDD_init_caches();
}

void
MR_ROBDD_init_caches(void)
{
    MR_ROBDD_INIT_CACHE(MR_ROBDD_ite);
#if defined(MR_ROBDD_USE_ITE_CONSTANT)
    MR_ROBDD_INIT_CACHE(MR_ROBDD_ite_constant);
#endif /* MR_ROBDD_USE_ITE_CONSTANT */
#if defined(MR_ROBDD_SHARING)
    MR_ROBDD_INIT_CACHE(MR_ROBDD_upclose);
    MR_ROBDD_INIT_CACHE(MR_ROBDD_bin);
    MR_ROBDD_INIT_CACHE(MR_ROBDD_complete);
#if defined(MR_ROBDD_NEW)
    MR_ROBDD_INIT_CACHE(MR_ROBDD_complete_one);
    MR_ROBDD_INIT_CACHE(MR_ROBDD_complete_or);
    MR_ROBDD_INIT_CACHE(MR_ROBDD_complete_one_or);
#endif /* MR_ROBDD_NEW */
    MR_ROBDD_INIT_CACHE(MR_ROBDD_bin);
#endif /* MR_ROBDD_SHARING */
    MR_ROBDD_INIT_CACHE(MR_ROBDD_glb);
    MR_ROBDD_INIT_CACHE(MR_ROBDD_lub);
#if defined(MR_ROBDD_USE_RGLB)
    MR_ROBDD_INIT_CACHE(rglb);
    MR_ROBDD_INIT_CACHE(MR_ROBDD_var_entailed);
#endif /* MR_ROBDD_USE_RGLB */
#if defined(MR_ROBDD_NEW)
    MR_ROBDD_INIT_CACHE(MR_ROBDD_ite_var);
    MR_ROBDD_INIT_CACHE(MR_ROBDD_vars_entailed);
#endif /* MR_ROBDD_NEW */
}

void
MR_ROBDD_concludeRep(void)
{
#if defined(MR_ROBDD_STATISTICS)
    MR_ROBDD_node *ptr;
    MR_ROBDD_int size_count[MR_ROBDD_MAX_COUNT+2];

    MR_ROBDD_int i, count;

    printf("\n\n\n================ Operation Counts ================\n\n");
    MR_ROBDD_PRINT_FN_COUNT(MR_ROBDD_make_node);
    MR_ROBDD_PRINT_FN_COUNT(MR_ROBDD_variableRep);
    MR_ROBDD_PRINT_FN_COUNT(MR_ROBDD_implies);
    MR_ROBDD_PRINT_FN_COUNT(MR_ROBDD_lub);
    MR_ROBDD_PRINT_FN_COUNT(MR_ROBDD_glb);
    MR_ROBDD_PRINT_FN_COUNT(MR_ROBDD_restrict);
    MR_ROBDD_PRINT_FN_COUNT(MR_ROBDD_restrictThresh);
    MR_ROBDD_PRINT_FN_COUNT(MR_ROBDD_renameArray);
    MR_ROBDD_PRINT_FN_COUNT(MR_ROBDD_reverseRenameArray);
    MR_ROBDD_PRINT_FN_COUNT(MR_ROBDD_ite);
#if defined(MR_ROBDD_USE_ITE_CONSTANT)
    MR_ROBDD_PRINT_FN_COUNT(MR_ROBDD_ite_constant);
#endif /* MR_ROBDD_USE_ITE_CONSTANT */
    MR_ROBDD_PRINT_FN_COUNT(iff_conj);
#if defined(MR_ROBDD_NEW)
    MR_ROBDD_PRINT_FN_COUNT(MR_ROBDD_ite_var);
    MR_ROBDD_PRINT_FN_COUNT(MR_ROBDD_vars_entailed);
#endif
#if defined(MR_ROBDD_SHARING)
  #if defined(MR_ROBDD_NEW)
        MR_ROBDD_PRINT_FN_COUNT(MR_ROBDD_complete_one_or);
        MR_ROBDD_PRINT_FN_COUNT(MR_ROBDD_complete_one);
        MR_ROBDD_PRINT_FN_COUNT(MR_ROBDD_complete_or);
  #endif /* MR_ROBDD_NEW */
        MR_ROBDD_PRINT_FN_COUNT(MR_ROBDD_complete);
        MR_ROBDD_PRINT_FN_COUNT(MR_ROBDD_upclose);
#endif /* MR_ROBDD_SHARING */
    printf("\n================ Cache Performance ================\n\n");
    printf("%d nodes extant\n\n", MR_ROBDD_nodes_in_use());
    printf("Unique table:  %ld hits, %ld misses, %f%% hits\n",
           (long) MR_ROBDD_unique_table_hits,
           (long) MR_ROBDD_unique_table_misses,
           MR_ROBDD_PERCENTAGE(MR_ROBDD_unique_table_hits,
              MR_ROBDD_unique_table_hits + MR_ROBDD_unique_table_misses));
    MR_memset(size_count, 0, sizeof(size_count));
    for (i=0; i<MR_ROBDD_UNIQUE_TABLE_SIZE; ++i) {
        count = 0;
        ptr=MR_ROBDD_REVEAL_NODE_POINTER(MR_ROBDD_unique_table[i]);
        while (ptr!=NULL) {
             ptr=MR_ROBDD_REVEAL_NODE_POINTER(ptr->unique),++count;
        }
        ++size_count[(count <= MR_ROBDD_MAX_COUNT
            ? count : MR_ROBDD_MAX_COUNT + 1)];
    }
    MR_ROBDD_print_distribution(size_count, MR_ROBDD_MAX_COUNT);

    MR_ROBDD_PRINT_CACHE_PERFORMANCE(MR_ROBDD_ite);
#if defined(MR_ROBDD_USE_ITE_CONSTANT)
    MR_ROBDD_PRINT_CACHE_PERFORMANCE(MR_ROBDD_ite_constant);
#endif /* MR_ROBDD_USE_ITE_CONSTANT */
    MR_ROBDD_PRINT_CACHE_PERFORMANCE(MR_ROBDD_lub);
    MR_ROBDD_PRINT_CACHE_PERFORMANCE(MR_ROBDD_glb);
#if defined(MR_ROBDD_SHARING)
    MR_ROBDD_PRINT_CACHE_PERFORMANCE(MR_ROBDD_upclose);
    MR_ROBDD_PRINT_CACHE_PERFORMANCE(MR_ROBDD_complete);
  #if defined(MR_ROBDD_NEW)
    MR_ROBDD_PRINT_CACHE_PERFORMANCE(MR_ROBDD_complete_or);
    MR_ROBDD_PRINT_CACHE_PERFORMANCE(MR_ROBDD_complete_one_or);
    MR_ROBDD_PRINT_CACHE_PERFORMANCE(MR_ROBDD_complete_one);
  #endif /* MR_ROBDD_NEW */
#endif /* MR_ROBDD_SHARING */
#if defined(MR_ROBDD_USE_RGLB)
    MR_ROBDD_PRINT_CACHE_PERFORMANCE(rglb);
#endif /* MR_ROBDD_USE_RGLB */
#if defined(MR_ROBDD_NEW)
    MR_ROBDD_PRINT_CACHE_PERFORMANCE(MR_ROBDD_ite_var);
    MR_ROBDD_PRINT_CACHE_PERFORMANCE(MR_ROBDD_vars_entailed);
#endif /* MR_ROBDD_NEW */
#endif /* MR_ROBDD_STATISTICS */
    }

/****************************************************************

               Testing Support

 ****************************************************************/

/*
** special version of MR_ROBDD_iff_conj_array() that is the fast version
** no matter which options are selected. I hope this gives more reliable
** test times.
*/

MR_ROBDD_node *
MR_ROBDD_testing_iff_conj_array(MR_ROBDD_int v0, MR_ROBDD_int n,
    MR_ROBDD_int arr[])
{
    MR_ROBDD_node *thens = MR_ROBDD_one;
    MR_ROBDD_node *elses = MR_ROBDD_zero;
    MR_ROBDD_int *ptr;
    MR_ROBDD_int vi;

    /*
    ** first build part of graph below v0.  For this, we build two
    ** subgraphs:  MR_ROBDD_one for when v0 is true, and MR_ROBDD_one for false.
    ** These are called thens and elses.
    **/

    for (ptr=&arr[n-1]; ptr>=arr && v0<(vi=*ptr); --ptr) {
        thens = MR_ROBDD_make_node(vi, thens, MR_ROBDD_zero);
        elses = MR_ROBDD_make_node(vi, elses, MR_ROBDD_one);
    }
    
    /* make v0 MR_ROBDD_node */
    thens = MR_ROBDD_make_node(v0, thens, elses);

    /*
    ** Finally build part of graph above v0.  For this, we build
    ** only MR_ROBDD_one graph, whose then branch is the graph we've built
    ** so far and whose else branch is ~v0.
    */

    if (ptr >= arr) {
        /* make ~v0 */
        elses = MR_ROBDD_make_node(v0, MR_ROBDD_zero, MR_ROBDD_one);

        do {
            vi = *ptr;
            thens = MR_ROBDD_make_node(vi, thens, elses);
        } while (--ptr >= arr);
    }

    return thens;
}
    
static int MR_ROBDD_intcompare(const void *a, const void *b)
{
    MR_ROBDD_int diff = (* ((MR_ROBDD_int *) a) - * ((MR_ROBDD_int *) b));
    if (diff > 0)
        return 1;
    else if (diff < 0)
        return -1;
    else
        return 0;
}
