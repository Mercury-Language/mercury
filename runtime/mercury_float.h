/*
** Copyright (C) 1995-1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_float.h - floating point handling */

#ifndef MERCURY_FLOAT_H
#define MERCURY_FLOAT_H

#include "conf.h"		/* for BOXED_FLOAT */
#include "mercury_types.h"	/* for `Word' */

#ifdef USE_SINGLE_PREC_FLOAT
typedef float Float;
#else
typedef double Float;
#endif

#ifdef BOXED_FLOAT 

#define word_to_float(w) (*(Float *)(w))

#define FLOAT_WORDS ((sizeof(Float) + sizeof(Word) - 1) / sizeof(Word))

#ifdef CONSERVATIVE_GC
#define float_to_word(f) ( \
		hp_alloc(FLOAT_WORDS), \
		*(Float *)(void *)(hp - FLOAT_WORDS) = (f), \
		/* return */ (Word) (hp - FLOAT_WORDS) \
	)
#else
/* we need to ensure that what we allocated on the heap is properly
   aligned */
#define float_to_word(f) ( \
		( (Word)hp & (sizeof(Float) - 1) ? hp_alloc(1) : (void)0 ), \
		hp_alloc(FLOAT_WORDS), \
		*(Float *)(void *)(hp - FLOAT_WORDS) = (f), \
		/* return */ (Word) (hp - FLOAT_WORDS) \
	)
#endif

#ifdef __GNUC__
#define float_const(f) ({ static const Float d = f; (Word)&d; })
#else
#define float_const(f) float_to_word(f)	/* inefficient */
#endif

#else /* not BOXED_FLOAT */

/* unboxed float means we can assume sizeof(Float) == sizeof(Word) */

union FloatWord {
	Float f;
	Word w;
};

#define float_const(f) float_to_word(f)

#ifdef __GNUC__

/* GNU C allows you to cast to a union type */
#define float_to_word(f) (__extension__ ((union FloatWord)(Float)(f)).w)
#define word_to_float(w) (__extension__ ((union FloatWord)(Word)(w)).f)

#else /* not __GNUC__ */

static Word float_to_word(Float f)
	{ union FloatWord tmp; tmp.f = f; return tmp.w; }
static Float word_to_float(Word w)
	{ union FloatWord tmp; tmp.w = w; return tmp.f; }

#endif /* not __GNUC__ */

#endif /* not BOXED_FLOAT */

Integer hash_float(Float);

#endif /* not MERCURY_FLOAT_H */
