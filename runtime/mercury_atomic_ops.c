/*
** vim:ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2007 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_atomic_ops.c
*/

#include "mercury_imp.h"
#include "mercury_atomic_ops.h"

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
