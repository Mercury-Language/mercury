/*
** Copyright (C) 1995-1998,2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_overflow.h - definitions for overflow checks */

#ifndef MERCURY_OVERFLOW_H
#define MERCURY_OVERFLOW_H

#define IF(cond, val)	((cond) ? ((val), (void)0) : (void)0)

#ifndef MR_CHECK_FOR_OVERFLOW

#define	MR_heap_overflow_check()	((void)0)
#define	MR_detstack_overflow_check()	((void)0)
#define	MR_detstack_underflow_check()	((void)0)
#define	MR_nondstack_overflow_check()	((void)0)
#define	MR_nondstack_underflow_check()	((void)0)

#else /* MR_CHECK_FOR_OVERFLOW */

#include "mercury_regs.h"
#include "mercury_misc.h"	/* for MR_fatal_error() */

#define	MR_heap_overflow_check()					\
		(							\
			IF (MR_hp >= MR_ENGINE(MR_heap_zone)->top,(	\
				MR_fatal_error("heap overflow")		\
			)),						\
			IF (MR_hp > MR_ENGINE(MR_heap_zone)->max,(	\
				MR_ENGINE(heap_zone)->max = MR_hp	\
			)),						\
			(void)0						\
		)

#define	MR_detstack_overflow_check()					\
		(							\
			IF (MR_sp >= MR_CONTEXT(MR_detstack_zone)->top,(\
				MR_fatal_error("stack overflow")	\
			)),						\
			IF (MR_sp > MR_CONTEXT(MR_detstack_zone)->max,(	\
				MR_CONTEXT(detstack_zone)->max = MR_sp	\
			)),						\
			(void)0						\
		)

#define	MR_detstack_underflow_check()					\
		(							\
			IF (MR_sp < MR_CONTEXT(MR_detstack_zone)->min,(	\
				MR_fatal_error("stack underflow")	\
			)),						\
			(void)0						\
		)

#define	MR_nondstack_overflow_check()					\
		(							\
			IF (MR_maxfr >= MR_CONTEXT(MR_nondetstack_zone)->top,( \
				MR_fatal_error("nondetstack overflow")	\
			)),						\
			IF (MR_maxfr > MR_CONTEXT(MR_nondetstack_zone)->max,( \
				MR_CONTEXT(nondetstack_zone)->max = MR_maxfr \
			)),						\
			(void)0						\
		)

#define	MR_nondstack_underflow_check()					\
		(							\
			IF (MR_maxfr < MR_CONTEXT(MR_nondetstack_zone)->min,( \
				MR_fatal_error("nondetstack underflow")	\
			)),						\
			(void)0						\
		)

#endif /* MR_CHECK_FOR_OVERFLOW */

#endif /* not MERCURY_OVERFLOW_H */
