/*
** vim:ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2003 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_term_size.h
**
** This module declares functions for returning the sizes of terms.
*/

#ifndef MR_MERCURY_TERM_SIZE_H
#define MR_MERCURY_TERM_SIZE_H

#include "mercury_types.h"

#ifdef  MR_RECORD_TERM_SIZES

extern  MR_Unsigned MR_term_size(MR_TypeInfo type_info, MR_Word term);

#else   /* MR_RECORD_TERM_SIZES */

/*
** Term sizes are not meaningful if MR_RECORD_TERM_SIZES is not defined.
** This macro, and others in mercury_heap.h, allows us to write code to
** compute term sizes without worrying about whether MR_RECORD_TERM_SIZES
** is defined or not.
*/

#define MR_term_size(type_info, term)       0

#endif  /* MR_RECORD_TERM_SIZES */

#endif  /* MR_MERCURY_TERM_SIZE_H */
