/*
** vim:ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2007, 2009 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_atomic_ops.c
*/

#include "mercury_imp.h"
#include "mercury_atomic_ops.h"

#if defined(MR_LL_PARALLEL_CONJ)

/*---------------------------------------------------------------------------*/
/*
** Provide definitions for functions declared `extern inline'.
*/

MR_OUTLINE_DEFN(
    MR_bool
    MR_compare_and_swap_word(volatile MR_Integer *addr, MR_Integer old,
        MR_Integer new_val)
,
    {
        MR_COMPARE_AND_SWAP_WORD_BODY;
    }
)

MR_OUTLINE_DEFN(
    void 
    MR_atomic_inc_int(volatile MR_Integer *addr)
,
    {
        MR_ATOMIC_INC_WORD_BODY;
    }
)

MR_OUTLINE_DEFN(
    void 
    MR_atomic_dec_int(volatile MR_Integer *addr)
,
    {
        MR_ATOMIC_DEC_WORD_BODY;
    }
)

#endif /* MR_LL_PARALLEL_CONJ */
