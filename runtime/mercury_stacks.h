/*
** Copyright (C) 1995-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_stacks.h - definitions for manipulation the det and nondet stacks */

#ifndef MERCURY_STACKS_H
#define MERCURY_STACKS_H

#include "mercury_regs.h"
#include "mercury_types.h"
#include "mercury_overflow.h"
#include "mercury_debug.h"
#include "mercury_goto.h"

/* DEFINITIONS FOR MANIPULATING THE DET STACK */

#define	detstackvar(n)	(MR_sp[-n])
#define	saved_detstackvar(save_area, n)	(MR_saved_sp(save_area)[-n])

#define	incr_sp_push_msg(n, msg)				\
			(					\
				debugincrsp(n, MR_sp),		\
				dump_push_msg(msg),		\
				MR_sp = MR_sp + (n),		\
				detstack_overflow_check(),	\
				(void)0				\
			)

#define	decr_sp_pop_msg(n)					\
			(					\
				debugdecrsp(n, MR_sp),		\
				dump_pop_msg(),			\
				MR_sp = MR_sp - (n),		\
				detstack_underflow_check(),	\
				(void)0				\
			)

#define	incr_sp(n)	(					\
				debugincrsp(n, MR_sp),		\
				MR_sp = MR_sp + (n),		\
				detstack_overflow_check(),	\
				(void)0				\
			)

#define	decr_sp(n)	(					\
				debugdecrsp(n, MR_sp),		\
				MR_sp = MR_sp - (n),		\
				detstack_underflow_check(),	\
				(void)0				\
			)

#define	push(w)		(					\
				*MR_sp = (Word) (w),		\
				debugpush(*sp, MR_sp),		\
				MR_sp = MR_sp + 1,		\
				detstack_overflow_check(),	\
				(void)0				\
			)

#define	pop()		(					\
				MR_sp = MR_sp - 1,		\
				debugpop(*MR_sp, MR_sp),	\
				detstack_underflow_check(),	\
				/* return */ *MR_sp		\
			)

/* DEFINITIONS FOR NONDET STACK FRAMES */

#define	REDOIP		(-0)	/* in this proc, set up at clause entry	*/
#define	PREVFR		(-1)	/* prev frame on stack, set up at call	*/
#define	SUCCIP		(-2)	/* in caller proc, set up at call	*/
#define	SUCCFR		(-3)	/* frame of caller proc, set up at call	*/

#ifdef MR_DEBUG_NONDET_STACK
  #define PREDNM		(-4)	/* for debugging, set up at call */
  #define bt_prednm(fr)	LVALUE_CAST(const char *, ((Word *) fr)[PREDNM])
  #define NONDET_FIXED_SIZE_0	5	/* units: words */
#else
  #define bt_prednm(fr)	"unknown"
  #define NONDET_FIXED_SIZE_0	4	/* units: words */
#endif

#define NONDET_FIXED_SIZE	NONDET_FIXED_SIZE_0

#define	SAVEVAL		(-NONDET_FIXED_SIZE)
			/* saved values start at this offset	*/

#define	bt_redoip(fr)	LVALUE_CAST(Code *, ((Word *) (fr))[REDOIP])
#define	bt_prevfr(fr)	LVALUE_CAST(Word *, ((Word *) (fr))[PREVFR])
#define	bt_succip(fr)	LVALUE_CAST(Code *, ((Word *) (fr))[SUCCIP])
#define	bt_succfr(fr)	LVALUE_CAST(Word *, ((Word *) (fr))[SUCCFR])
#define	bt_var(fr,n)	(((Word *) (fr))[SAVEVAL-(n)])

#define	curprednm	bt_prednm(MR_curfr)
#define	curredoip	bt_redoip(MR_curfr)
#define	curprevfr	bt_prevfr(MR_curfr)
#define	cursuccip	bt_succip(MR_curfr)
#define	cursuccfr	bt_succfr(MR_curfr)
#define	framevar(n)	bt_var(MR_curfr,n)

#define	saved_framevar(save_area, n)	bt_var(MR_saved_curfr(save_area), n)

/* DEFINITIONS FOR MANIPULATING THE NONDET STACK */

#ifdef	MR_DEBUG_NONDET_STACK
#define mkframe_save_prednm(prednm) (curprednm = prednm)
#else
#define mkframe_save_prednm(prednm) /* nothing */
#endif

#define	mkframe(prednm, numslots, redoip)			\
			do {					\
				reg	Word	*prevfr;	\
				reg	Word	*succfr;	\
								\
				prevfr = MR_maxfr;		\
				succfr = MR_curfr;		\
				MR_maxfr += (NONDET_FIXED_SIZE + numslots);\
				MR_curfr = MR_maxfr;		\
				curredoip = redoip;		\
				curprevfr = prevfr;		\
				cursuccip = MR_succip;		\
				cursuccfr = succfr;		\
				mkframe_save_prednm(prednm);	\
				debugmkframe();			\
				nondstack_overflow_check();	\
			} while (0)

/* just like mkframe, but also reserves space for a struct     */
/* with the given tag at the bottom of the nondet stack frame  */
#define	mkpragmaframe(prednm, numslots, structname, redoip)	\
			do {					\
				reg	Word	*prevfr;	\
				reg	Word	*succfr;	\
								\
				prevfr = MR_maxfr;		\
				succfr = MR_curfr;		\
				MR_maxfr += (NONDET_FIXED_SIZE + numslots \
					+ sizeof(struct structname));	\
				MR_curfr = MR_maxfr;		\
				curredoip = redoip;		\
				curprevfr = prevfr;		\
				cursuccip = MR_succip;		\
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
				childfr = MR_curfr;		\
				MR_curfr = cursuccfr;		\
				GOTO(bt_succip(childfr));	\
			} while (0)

#define	succeed_discard()					\
			do {					\
				reg	Word	*childfr;	\
								\
				debugsucceeddiscard();		\
				childfr = MR_curfr;		\
				MR_maxfr = curprevfr;		\
				MR_curfr = cursuccfr;		\
				GOTO(bt_succip(childfr));	\
			} while (0)

#define	fail()		do {					\
				debugfail();			\
				MR_maxfr = curprevfr;		\
				MR_curfr = MR_maxfr;			\
				nondstack_underflow_check();	\
				GOTO(curredoip);		\
			} while (0)

#define	redo()		do {					\
				debugredo();			\
				MR_curfr = MR_maxfr;			\
				GOTO(curredoip);		\
			} while (0)

#endif /* not MERCURY_STACKS_H */
