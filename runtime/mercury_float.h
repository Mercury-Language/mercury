/*
** Copyright (C) 1995-1997, 1999-2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_float.h - floating point handling */

#ifndef MERCURY_FLOAT_H
#define MERCURY_FLOAT_H

#include "mercury_conf.h"	/* for BOXED_FLOAT */
#include "mercury_types.h"	/* for `MR_Word' */

#ifdef USE_SINGLE_PREC_FLOAT
typedef float MR_Float;
#else
typedef double MR_Float;
#endif

#ifdef BOXED_FLOAT 

#define word_to_float(w) (*(MR_Float *)(w))

#define FLOAT_WORDS ((sizeof(MR_Float) + sizeof(MR_Word) - 1) / sizeof(MR_Word))

#ifdef CONSERVATIVE_GC
#define float_to_word(f) ( \
		hp_alloc_atomic(FLOAT_WORDS), \
		*(MR_Float *)(void *)(MR_hp - FLOAT_WORDS) = (f), \
		/* return */ (MR_Word) (MR_hp - FLOAT_WORDS) \
	)
#else
/* we need to ensure that what we allocated on the heap is properly
   aligned */
#define float_to_word(f) ( \
		( (MR_Word)MR_hp & (sizeof(MR_Float) - 1) ? \
			hp_alloc_atomic(1) : (void)0 ), \
		hp_alloc_atomic(FLOAT_WORDS), \
		*(MR_Float *)(void *)(MR_hp - FLOAT_WORDS) = (f), \
		/* return */ (MR_Word) (MR_hp - FLOAT_WORDS) \
	)
#endif

#ifdef __GNUC__
#define float_const(f) ({ static const MR_Float d = f; (MR_Word)&d; })
#else
#define float_const(f) float_to_word(f)	/* inefficient */
#endif

#else /* not BOXED_FLOAT */

/* unboxed float means we can assume sizeof(MR_Float) == sizeof(MR_Word) */

union MR_Float_Word {
	MR_Float f;
	MR_Word w;
};

#define float_const(f) float_to_word(f)

#ifdef __GNUC__

/* GNU C allows you to cast to a union type */
#define float_to_word(f) (__extension__ ((union MR_Float_Word)(MR_Float)(f)).w)
#define word_to_float(w) (__extension__ ((union MR_Float_Word)(MR_Word)(w)).f)

#else /* not __GNUC__ */

static MR_Word float_to_word(MR_Float f)
	{ union MR_Float_Word tmp; tmp.f = f; return tmp.w; }
static MR_Float word_to_float(MR_Word w)
	{ union MR_Float_Word tmp; tmp.w = w; return tmp.f; }

#endif /* not __GNUC__ */

#endif /* not BOXED_FLOAT */

MR_Integer hash_float(MR_Float);

#endif /* not MERCURY_FLOAT_H */
