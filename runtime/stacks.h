/*
** Copyright (C) 1995-1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* stacks.h - definitions for manipulation the det and nondet stacks */

#ifndef STACKS_H
#define STACKS_H

#include "regs.h"
#include "mercury_types.h"
#include "overflow.h"
#include "debug.h"
#include "goto.h"

/* DEFINITIONS FOR MANIPULATING THE DET STACK */

#define	detstackvar(n)	sp[-n]

#define	incr_sp_push_msg(n, msg)				\
			(					\
				debugincrsp(n, sp),		\
				dump_push_msg(msg),		\
				sp = sp + (n),			\
				detstack_overflow_check(),	\
				(void)0				\
			)

#define	decr_sp_pop_msg(n)					\
			(					\
				debugdecrsp(n, sp),		\
				dump_pop_msg(),			\
				sp = sp - (n),			\
				detstack_underflow_check(),	\
				(void)0				\
			)

#define	incr_sp(n)	(					\
				debugincrsp(n, sp),		\
				sp = sp + (n),			\
				detstack_overflow_check(),	\
				(void)0				\
			)

#define	decr_sp(n)	(					\
				debugdecrsp(n, sp),		\
				sp = sp - (n),			\
				detstack_underflow_check(),	\
				(void)0				\
			)

#define	push(w)		(					\
				*sp = (Word) (w),		\
				debugpush(*sp, sp),		\
				sp = sp + 1,			\
				detstack_overflow_check(),	\
				(void)0				\
			)

#define	pop()		(					\
				sp = sp - 1,			\
				debugpop(*sp, sp),		\
				detstack_underflow_check(),	\
				/* return */ *sp		\
			)

/* DEFINITIONS FOR NONDET STACK FRAMES */

#define	REDOIP		(-0)	/* in this proc, set up at clause entry	*/
#define	PREVFR		(-1)	/* prev frame on stack, set up at call	*/
#define	SUCCIP		(-2)	/* in caller proc, set up at call	*/
#define	SUCCFR		(-3)	/* frame of caller proc, set up at call	*/

#ifdef	SPEED
#define	bt_prednm(fr)	"unknown"
#define	NONDET_FIXED_SIZE_0	4	/* units: words */
#else
#define	PREDNM		(-4)	/* for debugging, set up at call 	*/
#define	bt_prednm(fr)	LVALUE_CAST(const char *, ((Word *) fr)[PREDNM])
#define	NONDET_FIXED_SIZE_0	5	/* units: words */
#endif

#define NONDET_FIXED_SIZE	NONDET_FIXED_SIZE_0

#define	SAVEVAL		(-NONDET_FIXED_SIZE)
			/* saved values start at this offset	*/

#define	bt_redoip(fr)	LVALUE_CAST(Code *, ((Word *) (fr))[REDOIP])
#define	bt_prevfr(fr)	LVALUE_CAST(Word *, ((Word *) (fr))[PREVFR])
#define	bt_succip(fr)	LVALUE_CAST(Code *, ((Word *) (fr))[SUCCIP])
#define	bt_succfr(fr)	LVALUE_CAST(Word *, ((Word *) (fr))[SUCCFR])
#define	bt_var(fr,n)	(((Word *) (fr))[SAVEVAL-(n)])

#define	curprednm	bt_prednm(curfr)
#define	curredoip	bt_redoip(curfr)
#define	curprevfr	bt_prevfr(curfr)
#define	cursuccip	bt_succip(curfr)
#define	cursuccfr	bt_succfr(curfr)
#define	framevar(n)	bt_var(curfr,n)

/* DEFINITIONS FOR MANIPULATING THE NONDET STACK */

#ifndef	SPEED
#define mkframe_save_prednm(prednm) (curprednm = prednm)
#else
#define mkframe_save_prednm(prednm) /* nothing */
#endif


#define	mkframe(prednm, numslots, redoip)				\
			do {					\
				reg	Word	*prevfr;	\
				reg	Word	*succfr;	\
								\
				prevfr = maxfr;			\
				succfr = curfr;			\
				maxfr += (NONDET_FIXED_SIZE + numslots);\
				curfr = maxfr;			\
				curredoip = redoip;		\
				curprevfr = prevfr;		\
				cursuccip = succip;		\
				cursuccfr = succfr;		\
				mkframe_save_prednm(prednm);	\
				debugmkframe();			\
				nondstack_overflow_check();	\
			} while (0)



#define	modframe(redoip)					\
			do {					\
				curredoip = redoip;		\
				debugmodframe();		\
			} while (0)


#define	succeed()	do {					\
				reg	Word	*childfr;	\
								\
				debugsucceed();			\
				childfr = curfr;		\
				curfr = cursuccfr;		\
				GOTO(bt_succip(childfr));	\
			} while (0)

#define	succeed_discard()					\
			do {					\
				reg	Word	*childfr;	\
								\
				debugsucceeddiscard();		\
				childfr = curfr;		\
				maxfr = curprevfr;		\
				curfr = cursuccfr;		\
				GOTO(bt_succip(childfr));	\
			} while (0)


#define	fail()		do {					\
				debugfail();			\
				maxfr = curprevfr;		\
				curfr = maxfr;			\
				nondstack_underflow_check();	\
				GOTO(curredoip);		\
			} while (0)


#define	redo()		do {					\
				debugredo();			\
				curfr = maxfr;			\
				GOTO(curredoip);		\
			} while (0)

#endif /* not STACKS_H */
