/*
** Copyright (C) 1995-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_stacks.h - definitions for manipulating the det and nondet stacks */

#ifndef MERCURY_STACKS_H
#define MERCURY_STACKS_H

#include "mercury_regs.h"
#include "mercury_types.h"
#include "mercury_overflow.h"
#include "mercury_debug.h"
#include "mercury_goto.h"

/* DEFINITIONS FOR MANIPULATING THE DET STACK */

#define	MR_based_stackvar(base_sp, n)	((base_sp)[-n])
#define	MR_stackvar(n)			MR_based_stackvar(MR_sp, n)

#define	detstackvar(n)			MR_based_stackvar(MR_sp, n)
#define	based_detstackvar(base_sp, n)	MR_based_stackvar(base_sp, n)
#define	saved_detstackvar(save_area, n)	\
			MR_based_stackvar(MR_saved_sp(save_area), n)

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
				debugpush(*MR_sp, MR_sp),	\
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

#define	MR_PREVFR	(-0)	/* prev frame on stack, set up at call	*/
#define	MR_REDOIP	(-1)	/* in this proc, set up at clause entry	*/
#define	MR_REDOFR	(-2)	/* value for curfr on backtracking      */
#define	MR_SUCCIP	(-3)	/* in caller proc, set up at call	*/
#define	MR_SUCCFR	(-4)	/* frame of caller proc, set up at call	*/
#define	MR_DETFR	(-3)	/* sp, in model_det temp frames only	*/

#ifdef MR_DEBUG_NONDET_STACK
  #define MR_PREDNM		(-5)	/* for debugging, set up at call */
  #define MR_prednm_slot(fr)	LVALUE_CAST(const char *, \
		  			((Word *) fr)[MR_PREDNM])
  #define MR_NONDET_FIXED_SIZE	6	/* units: words */
#else
  #define MR_prednm_slot(fr)	"unknown"
  #define MR_NONDET_FIXED_SIZE	5	/* units: words */
#endif

/*
** Code that traverses the nondet stack depends on the relationship
** MR_NONDET_TEMP_SIZE < MR_DET_TEMP_SIZE < MR_NONDET_FIXED_SIZE.
*/

#define	MR_NONDET_TEMP_SIZE	3	/* prevfr, redoip, redofr */
#define	MR_DET_TEMP_SIZE	4	/* prevfr, redoip, redofr, detfr */

#define	MR_SAVEVAL			(-MR_NONDET_FIXED_SIZE)
				/* saved values start at this offset	*/

#define	MR_prevfr_slot(fr)	LVALUE_CAST(Word *, ((Word *) (fr))[MR_PREVFR])
#define	MR_redoip_slot(fr)	LVALUE_CAST(Code *, ((Word *) (fr))[MR_REDOIP])
#define	MR_redofr_slot(fr)	LVALUE_CAST(Word *, ((Word *) (fr))[MR_REDOFR])
#define	MR_succip_slot(fr)	LVALUE_CAST(Code *, ((Word *) (fr))[MR_SUCCIP])
#define	MR_succfr_slot(fr)	LVALUE_CAST(Word *, ((Word *) (fr))[MR_SUCCFR])
#define	MR_detfr_slot(fr)	LVALUE_CAST(Word *, ((Word *) (fr))[MR_DETFR])
#define	MR_based_framevar(fr,n)	(((Word *) (fr))[MR_SAVEVAL+1-(n)])

#define	bt_prevfr(fr)		MR_prevfr_slot(fr)
#define	bt_redoip(fr)		MR_redoip_slot(fr)
#define	bt_redofr(fr)		MR_redofr_slot(fr)
#define	bt_succip(fr)		MR_succip_slot(fr)
#define	bt_succfr(fr)		MR_succfr_slot(fr)
#define	bt_prednm(fr)		MR_prednm_slot(fr)
#define	bt_var(fr,n)		MR_based_framevar(fr,n+1)

#define	curprevfr		bt_prevfr(MR_curfr)
#define	curredoip		bt_redoip(MR_curfr)
#define	curredofr		bt_redofr(MR_curfr)
#define	cursuccip		bt_succip(MR_curfr)
#define	cursuccfr		bt_succfr(MR_curfr)
#define	curprednm		bt_prednm(MR_curfr)

#define	MR_framevar(n)		MR_based_framevar(MR_curfr,n)
#define	framevar(n)		MR_framevar(n+1)
#define	based_framevar(base_curfr, n)	\
			MR_based_framevar(base_curfr, n+1)
#define	saved_framevar(save_area, n)	\
			MR_based_framevar(MR_saved_curfr(save_area), n+1)

/* DEFINITIONS FOR MANIPULATING THE NONDET STACK */

#ifdef MR_DEBUG_NONDET_STACK
  #define mkframe_save_prednm(prednm) (MR_prednm_slot(MR_curfr) = prednm)
#else
  #define mkframe_save_prednm(prednm) /* nothing */
#endif

#define	mkframe(prednm, numslots, redoip)				\
			do {						\
				reg	Word	*prevfr;		\
				reg	Word	*succfr;		\
									\
				prevfr = MR_maxfr;			\
				succfr = MR_curfr;			\
				MR_maxfr += (MR_NONDET_FIXED_SIZE + numslots);\
				MR_curfr = MR_maxfr;			\
				MR_redoip_slot(MR_curfr) = redoip;	\
				MR_prevfr_slot(MR_curfr) = prevfr;	\
				MR_succip_slot(MR_curfr) = MR_succip;	\
				MR_succfr_slot(MR_curfr) = succfr;	\
				MR_redofr_slot(MR_curfr) = MR_curfr;	\
				mkframe_save_prednm(prednm);		\
				debugmkframe();				\
				nondstack_overflow_check();		\
			} while (0)

/* just like mkframe, but also reserves space for a struct     */
/* with the given tag at the bottom of the nondet stack frame  */
#define	mkpragmaframe(prednm, numslots, structname, redoip)		\
			do {						\
				reg	Word	*prevfr;		\
				reg	Word	*succfr;		\
									\
				prevfr = MR_maxfr;			\
				succfr = MR_curfr;			\
				MR_maxfr += (MR_NONDET_FIXED_SIZE + numslots \
					+ sizeof(struct structname));	\
				MR_curfr = MR_maxfr;			\
				MR_redoip_slot(MR_curfr) = redoip;	\
				MR_prevfr_slot(MR_curfr) = prevfr;	\
				MR_succip_slot(MR_curfr) = MR_succip;	\
				MR_succfr_slot(MR_curfr) = succfr;	\
				MR_redofr_slot(MR_curfr) = MR_curfr;	\
				mkframe_save_prednm(prednm);		\
				debugmkframe();				\
				nondstack_overflow_check();		\
			} while (0)

#define	mktempframe(redoip)						\
			do {						\
				reg	Word	*prevfr;		\
				reg	Word	*succfr;		\
									\
				prevfr = MR_maxfr;			\
				succfr = MR_curfr;			\
				MR_maxfr += MR_NONDET_TEMP_SIZE;	\
				MR_prevfr_slot(MR_maxfr) = prevfr;	\
				MR_redoip_slot(MR_maxfr) = redoip;	\
				MR_redofr_slot(MR_maxfr) = MR_curfr;	\
				nondstack_overflow_check();		\
			} while (0)

#define	mkdettempframe(redoip)						\
			do {						\
				reg	Word	*prevfr;		\
				reg	Word	*succfr;		\
									\
				prevfr = MR_maxfr;			\
				succfr = MR_curfr;			\
				MR_maxfr += MR_DET_TEMP_SIZE;		\
				MR_prevfr_slot(MR_maxfr) = prevfr;	\
				MR_redoip_slot(MR_maxfr) = redoip;	\
				MR_redofr_slot(MR_maxfr) = MR_curfr;	\
				MR_detfr_slot(MR_maxfr)  = MR_sp;	\
				nondstack_overflow_check();		\
			} while (0)

/*
** This should be removed soon - the latest compiler does not generate it.
*/
#define modframe(redoip)						\
			do {						\
				curredoip = redoip;			\
			} while (0)

#define	succeed()	do {						\
				reg	Word	*childfr;		\
									\
				debugsucceed();				\
				childfr = MR_curfr;			\
				MR_curfr = MR_succfr_slot(childfr);	\
				GOTO(MR_succip_slot(childfr));		\
			} while (0)

#define	succeed_discard()						\
			do {						\
				reg	Word	*childfr;		\
									\
				debugsucceeddiscard();			\
				childfr = MR_curfr;			\
				MR_maxfr = MR_prevfr_slot(childfr);	\
				MR_curfr = MR_succfr_slot(childfr);	\
				GOTO(MR_succip_slot(childfr));		\
			} while (0)


#define	fail()		do {						\
				debugfail();				\
				MR_maxfr = MR_prevfr_slot(MR_maxfr);	\
				nondstack_underflow_check();		\
				MR_curfr = MR_redofr_slot(MR_maxfr);	\
				GOTO(MR_redoip_slot(MR_maxfr));		\
			} while (0)


#define	redo()		do {						\
				debugredo();				\
				MR_curfr = MR_redofr_slot(MR_maxfr);	\
				GOTO(MR_redoip_slot(MR_maxfr));		\
			} while (0)

#endif /* not MERCURY_STACKS_H */
