/*
** Copyright (C) 1995-1998,2000-2001, 2005 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_overflow.h - definitions for overflow checks */

#ifndef MERCURY_OVERFLOW_H
#define MERCURY_OVERFLOW_H

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
	(								\
		MR_IF (MR_hp >= MR_ENGINE(MR_eng_heap_zone)->top,(	\
			MR_fatal_error("heap overflow")			\
		)),							\
		MR_IF (MR_hp > MR_ENGINE(MR_eng_heap_zone)->max,(	\
			MR_ENGINE(MR_eng_heap_zone)->max = MR_hp	\
		)),							\
		(void)0							\
	)

#define	MR_detstack_overflow_check()					\
	(								\
		MR_IF (MR_sp >= MR_CONTEXT(MR_ctxt_detstack_zone)->top,(\
			MR_fatal_error("stack overflow")		\
		)),							\
		MR_IF (MR_sp > MR_CONTEXT(MR_ctxt_detstack_zone)->max,(	\
			MR_CONTEXT(MR_ctxt_detstack_zone)->max = MR_sp	\
		)),							\
		(void)0							\
	)

#define	MR_detstack_underflow_check()					\
	(								\
		MR_IF (MR_sp < MR_CONTEXT(MR_ctxt_detstack_zone)->min,(	\
			MR_fatal_error("stack underflow")		\
		)),							\
		(void)0							\
	)

#define	MR_nondstack_overflow_check()					\
	(								\
		MR_IF (MR_maxfr >= MR_CONTEXT(MR_ctxt_nondetstack_zone)->top,( \
			MR_fatal_error("nondetstack overflow")		\
		)),							\
		MR_IF (MR_maxfr > MR_CONTEXT(MR_ctxt_nondetstack_zone)->max,( \
			MR_CONTEXT(MR_ctxt_nondetstack_zone)->max = MR_maxfr\
		)),							\
		(void)0							\
	)

#define	MR_nondstack_underflow_check()					\
	(								\
		MR_IF (MR_maxfr < MR_CONTEXT(MR_ctxt_nondetstack_zone)->min,( \
			MR_fatal_error("nondetstack underflow")		\
		)),							\
		(void)0							\
	)

#endif /* MR_CHECK_FOR_OVERFLOW */

#endif /* not MERCURY_OVERFLOW_H */
