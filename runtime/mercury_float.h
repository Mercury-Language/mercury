/*
** Copyright (C) 1995-1997, 1999-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_float.h - floating point handling */

#ifndef MERCURY_FLOAT_H
#define MERCURY_FLOAT_H

#include "mercury_conf.h"	/* for MR_BOXED_FLOAT, MR_CONSERVATIVE_GC */
#include "mercury_types.h"	/* for `MR_Word' */
#include "mercury_std.h"	/* for `MR_bool' */

#ifdef MR_USE_SINGLE_PREC_FLOAT
  typedef float MR_Float;
  #define MR_FLT_MIN_PRECISION	7
  #define MR_FLT_FMT		"%f"
#else
  typedef double MR_Float;
  #define MR_FLT_MIN_PRECISION	15
  #define MR_FLT_FMT		"%lf"
#endif
#define MR_FLT_MAX_PRECISION	(MR_FLT_MIN_PRECISION + 2)

#ifdef MR_BOXED_FLOAT 

#define MR_word_to_float(w)	(* (MR_Float *) (w))

#define MR_FLOAT_WORDS		((sizeof(MR_Float) + sizeof(MR_Word) - 1) \
					/ sizeof(MR_Word))

#ifdef MR_CONSERVATIVE_GC
  #define MR_float_to_word(f) ( \
		MR_hp_alloc_atomic(MR_FLOAT_WORDS), \
		* (MR_Float *) (void *) (MR_hp - MR_FLOAT_WORDS) = (f), \
		/* return */ (MR_Word) (MR_hp - MR_FLOAT_WORDS) \
	)
  #define MR_make_hp_float_aligned() ((void)0)
#else
  /*
  ** We need to ensure that what we allocated on the heap is properly
  ** aligned for a floating-point value, by rounding MR_hp up to the
  ** nearest float-aligned boundary.
  ** XXX This code assumes that sizeof(MR_Float) is a power of two,
  ** and not greater than 2 * sizeof(MR_Word).
  */
  #define MR_make_hp_float_aligned() ( \
		( (MR_Word) MR_hp & (sizeof(MR_Float) - 1) ? \
			MR_hp_alloc_atomic(1) : (void)0 ) \
	)
  #define MR_float_to_word(f) ( \
		MR_make_hp_float_aligned(), \
		MR_hp_alloc_atomic(MR_FLOAT_WORDS), \
		* (MR_Float *) (void *)(MR_hp - MR_FLOAT_WORDS) = (f), \
		/* return */ (MR_Word) (MR_hp - MR_FLOAT_WORDS) \
	)
#endif

#ifdef __GNUC__
  #define MR_float_const(f) ({ static const MR_Float d = f; (MR_Word) &d; })
#else
  #define MR_float_const(f) MR_float_to_word(f)	/* inefficient */
#endif

#else /* not MR_BOXED_FLOAT */

  /* unboxed float means we can assume sizeof(MR_Float) == sizeof(MR_Word) */

  #define MR_make_hp_float_aligned() ((void)0)

  union MR_Float_Word {
	MR_Float f;
	MR_Word w;
  };

  #define MR_float_const(f) MR_float_to_word(f)

  #ifdef __GNUC__

    /* GNU C allows you to cast to a union type */
    #define MR_float_to_word(f) (__extension__ \
				((union MR_Float_Word)(MR_Float)(f)).w)
    #define MR_word_to_float(w) (__extension__ \
				((union MR_Float_Word)(MR_Word)(w)).f)

  #else /* not __GNUC__ */

    static MR_Word MR_float_to_word(MR_Float f)
	{ union MR_Float_Word tmp; tmp.f = f; return tmp.w; }
    static MR_Float MR_word_to_float(MR_Word w)
	{ union MR_Float_Word tmp; tmp.w = w; return tmp.f; }

  #endif /* not __GNUC__ */

#endif /* not MR_BOXED_FLOAT */

/*
** The size of the buffer to pass to MR_sprintf_float.
**
** Longest possible string for %#.*g format is `-n.nnnnnnE-mmmm', which
** has size  PRECISION + MAX_EXPONENT_DIGITS + 5 (for the `-', `.', `E',
** '-', and '\\0').  PRECISION is at most 20, and MAX_EXPONENT_DIGITS is
** at most 5, so we need at most 30 chars.  80 is way more than enough.
*/
#define MR_SPRINTF_FLOAT_BUF_SIZE   80

#define MR_float_to_string(Float, String)			\
	do {							\
		char buf[MR_SPRINTF_FLOAT_BUF_SIZE];		\
		MR_sprintf_float(buf, Float);			\
		MR_make_aligned_string_copy(Str, buf);		\
	} while (0)

void MR_sprintf_float(char *buf, MR_Float f);

MR_Integer MR_hash_float(MR_Float);

MR_bool MR_is_nan(MR_Float);
MR_bool MR_is_inf(MR_Float);

#endif /* not MERCURY_FLOAT_H */
