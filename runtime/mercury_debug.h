/*
** Copyright (C) 1995-2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_debug.h - definitions for debugging messages */

#ifndef MERCURY_DEBUG_H
#define MERCURY_DEBUG_H

#include "mercury_types.h"	/* for `Word' and `Code' */
#include <stdio.h>		/* for `FILE' */

/*---------------------------------------------------------------------------*/

#ifdef DEBUG_ON
	#define DEBUG(X) X
#else
	#define DEBUG(X)
#endif

#if !defined(MR_DEBUG_GOTOS)

#define	debuggoto(label)			((void)0)
#define	debugsreg()				((void)0)

#else

#define	debuggoto(label) \
	(MR_assert(label), \
	IF (MR_gotodebug, (save_transient_registers(), goto_msg(label))))

#define	debugsreg() \
	IF (MR_sregdebug, (save_transient_registers(), reg_msg()))

#endif

#ifndef MR_LOWLEVEL_DEBUG

#define	dump_push_msg(msg)			((void)0)
#define	dump_pop_msg()				((void)0)

#define	debugcr1(val0, hp)			((void)0)
#define	debugcr2(val0, val1, hp)		((void)0)
#define	debugincrhp(val, hp)			((void)0)
#define	debugincrsp(val, sp)			((void)0)
#define	debugdecrsp(val, sp)			((void)0)
#define	debugpush(val, sp)			((void)0)
#define	debugpop(val, sp)			((void)0)
#define	debugregs(msg)				((void)0)
#define	debugframe(msg)				((void)0)
#define	debugmkframe(predname)			((void)0)
#define	debugsucceed()				((void)0)
#define	debugsucceeddiscard()			((void)0)
#define	debugfail()				((void)0)
#define	debugredo()				((void)0)
#define	debugcall(proc, succ_cont)		((void)0)
#define	debugtailcall(proc)			((void)0)
#define	debugproceed()				((void)0)
#define	debugmsg0(msg)				((void)0)
#define	debugmsg1(msg, arg1)			((void)0)
#define	debugmsg2(msg, arg1, arg2)		((void)0)
#define	debugmsg3(msg, arg1, arg2, arg3)	((void)0)

#else

#define	dump_push_msg(msg)	\
	(((const char **)dumpstack_zone->min)[dumpindex++] = msg)
#define	dump_pop_msg()				(--dumpindex)

#define	debugcr1(val0, hp) \
	IF (MR_heapdebug, (save_transient_registers(), cr1_msg(val0, hp)))

#define	debugcr2(val0, val1, hp) \
	IF (MR_heapdebug, (save_transient_registers(), cr2_msg(val0, val1, hp)))

#define	debugincrhp(val, hp) \
	IF (MR_heapdebug, \
		(save_transient_registers(), incr_hp_debug_msg((val), (hp))))

#define	debugincrsp(val, sp) \
	IF (MR_detstackdebug, \
		(save_transient_registers(), incr_sp_msg((val), (sp))))

#define	debugdecrsp(val, sp) \
	IF (MR_detstackdebug, \
		(save_transient_registers(), decr_sp_msg((val), (sp))))

#define	debugpush(val, sp) \
	IF (MR_detstackdebug, \
		(save_transient_registers(), push_msg((val), (sp))))

#define	debugpop(val, sp) \
	IF (MR_detstackdebug, (save_transient_registers(), pop_msg(val, sp)))

#define	debugregs(msg) \
	IF (MR_progdebug, (save_transient_registers(), printregs(msg)))

#define	debugmkframe(predname) \
	IF (MR_nondstackdebug, \
		(save_transient_registers(), mkframe_msg(predname)))

#define	debugframe(msg)	 \
	IF (MR_progdebug, (save_transient_registers(), printframe(msg)))

#define	debugsucceed() \
	IF (MR_nondstackdebug, (save_transient_registers(), succeed_msg()))

#define	debugsucceeddiscard() \
	IF (MR_nondstackdebug, \
		(save_transient_registers(), succeeddiscard_msg()))

#define	debugfail() \
	IF (MR_nondstackdebug, (save_transient_registers(), fail_msg()))

#define	debugredo() \
	IF (MR_nondstackdebug, (save_transient_registers(), redo_msg()))

#define	debugcall(proc, succ_cont) \
	IF (MR_calldebug, \
		(save_transient_registers(), call_msg(proc, succ_cont)))

#define	debugtailcall(proc) \
	IF (MR_calldebug, (save_transient_registers(), tailcall_msg(proc)))

#define	debugproceed() \
	IF (MR_calldebug, (save_transient_registers(), proceed_msg()))

#define	debugmsg0(msg) \
	IF (MR_progdebug, (printf(msg)))

#define	debugmsg1(msg, arg1) \
	IF (MR_progdebug, (printf(msg, arg1)))

#define	debugmsg2(msg, arg1, arg2) \
	IF (MR_progdebug, (printf(msg, arg1, arg2)))

#define	debugmsg3(msg, arg1, arg2, arg3) \
	IF (MR_progdebug, (printf(msg, arg1, arg2, arg3)))

#endif /* MR_LOWLEVEL_DEBUG */

/*---------------------------------------------------------------------------*/

#ifdef MR_LOWLEVEL_DEBUG
extern	void	mkframe_msg(const char *);
extern	void	succeed_msg(void);
extern	void	succeeddiscard_msg(void);
extern	void	fail_msg(void);
extern	void	redo_msg(void);
extern	void	call_msg(/* const */ Code *proc, /* const */ Code *succcont);
extern	void	tailcall_msg(/* const */ Code *proc);
extern	void	proceed_msg(void);
extern	void	cr1_msg(Word val0, const Word *addr);
extern	void	cr2_msg(Word val0, Word val1, const Word *addr);
extern	void	incr_hp_debug_msg(Word val, const Word *addr);
extern	void	incr_sp_msg(Word val, const Word *addr);
extern	void	decr_sp_msg(Word val, const Word *addr);
extern	void	push_msg(Word val, const Word *addr);
extern	void	pop_msg(Word val, const Word *addr);
#endif

#ifdef MR_DEBUG_GOTOS
extern	void	goto_msg(/* const */ Code *addr);
extern	void	reg_msg(void);
#endif

#ifdef MR_LOWLEVEL_DEBUG
extern	void	printint(Word n);
extern	void	printstring(const char *s);
extern	void	printheap(const Word *h);
extern	void	dumpframe(/* const */ Word *);
extern	void	dumpnondstack(void);
extern	void	printlist(Word p);
extern	void	printframe(const char *);
extern	void	printregs(const char *msg);
#endif

extern	void	printdetstack(const Word *s);
extern	void	MR_printdetstackptr(const Word *s);
extern	void	MR_print_detstackptr(FILE *fp, const Word *s);
extern	void	printnondstack(const Word *s);
extern	void	MR_printnondstackptr(const Word *s);
extern	void	MR_print_nondstackptr(FILE *fp, const Word *s);
extern	void	MR_print_heapptr(FILE *fp, const Word *s);
extern	void	MR_print_label(FILE *fp, /* const */ Code *w);
extern	void	printlabel(/* const */ Code *w);

/*---------------------------------------------------------------------------*/

#endif /* not MERCURY_DEBUG_H */
