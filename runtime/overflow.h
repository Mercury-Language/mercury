/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* DEFINITIONS FOR OVERFLOW CHECKS */

#define IF(cond, val)	((cond) ? (val),(void)0 : (void)0)

#ifdef	SPEED

#define	heap_overflow_check()		((void)0)
#define	detstack_overflow_check()	((void)0)
#define	detstack_underflow_check()	((void)0)
#define	nondstack_overflow_check()	((void)0)
#define	nondstack_underflow_check()	((void)0)

#else

#define	heap_overflow_check()					\
			(					\
				IF (hp >= heap_zone->top,(	\
					fatal_error("heap overflow") \
				)),				\
				IF (hp > heap_zone->max,(	\
					heap_zone->max = hp	\
				)),				\
				(void)0				\
			)

#define	detstack_overflow_check()				\
			(					\
				IF (sp >= detstack_zone->top,(	\
					fatal_error("stack overflow") \
				)),				\
				IF (sp > detstack_zone->max,(	\
					detstack_zone->max = sp	\
				)),				\
				(void)0				\
			)

#define	detstack_underflow_check()				\
			(					\
				IF (sp < detstack_zone->min,(		\
					fatal_error("stack underflow") \
				)),				\
				(void)0				\
			)

#define	nondstack_overflow_check()				\
			(					\
				IF (maxfr >= nondetstack_zone->top,(	\
					fatal_error("nondetstack overflow") \
				)),				\
				IF (maxfr > nondetstack_zone->max,(	\
					nondetstack_zone->max = maxfr	\
				)),				\
				(void)0				\
			)

#define	nondstack_underflow_check()				\
			(					\
				IF (maxfr < nondetstack_zone->min,(	\
					fatal_error("nondetstack underflow") \
				)),				\
				(void)0				\
			)

#endif
