/*
** Copyright (C) 1995-1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* debug.h - definitions for debugging messages */

#ifndef DEBUG_H
#define DEBUG_H

#ifdef DEBUG_ON
	#define DEBUG(X) X
#else
	#define DEBUG(X)
#endif

#endif

#if defined(SPEED) && !defined(DEBUG_GOTOS)

#define	debuggoto(label)			((void)0)
#define	debugsreg()				((void)0)

#else

#define	debuggoto(label) \
	(MR_assert(label), \
	IF (gotodebug, (save_transient_registers(), goto_msg(label))))

#define	debugsreg() \
	IF (sregdebug, (save_transient_registers(), reg_msg()))

#endif

#ifdef	SPEED

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
#define	debugmkframe()				((void)0)
#define	debugmodframe()				((void)0)
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
	IF (heapdebug, (save_transient_registers(), cr1_msg(val0, hp)))

#define	debugcr2(val0, val1, hp) \
	IF (heapdebug, (save_transient_registers(), cr2_msg(val0, val1, hp)))

#define	debugincrhp(val, hp) \
	IF (heapdebug, (save_transient_registers(), incr_hp_msg((val), (hp))))

#define	debugincrsp(val, sp) \
	IF (detstackdebug, (save_transient_registers(), incr_sp_msg((val), (sp))))

#define	debugdecrsp(val, sp) \
	IF (detstackdebug, (save_transient_registers(), decr_sp_msg((val), (sp))))

#define	debugpush(val, sp) \
	IF (detstackdebug, (save_transient_registers(), push_msg((val), (sp))))

#define	debugpop(val, sp) \
	IF (detstackdebug, (save_transient_registers(), pop_msg(val, sp)))

#define	debugregs(msg) \
	IF (progdebug, (save_transient_registers(), printregs(msg)))

#define	debugmkframe() \
	IF (nondstackdebug, (save_transient_registers(), mkframe_msg()))

#define	debugframe(msg)	 \
	IF (progdebug, (save_transient_registers(), printframe(msg)))

#define	debugmodframe() \
	IF (nondstackdebug, (save_transient_registers(), modframe_msg()))

#define	debugsucceed() \
	IF (nondstackdebug, (save_transient_registers(), succeed_msg()))

#define	debugsucceeddiscard() \
	IF (nondstackdebug, (save_transient_registers(), succeeddiscard_msg()))

#define	debugfail() \
	IF (nondstackdebug, (save_transient_registers(), fail_msg()))

#define	debugredo() \
	IF (nondstackdebug, (save_transient_registers(), redo_msg()))

#define	debugcall(proc, succ_cont) \
	IF (calldebug, (save_transient_registers(), call_msg(proc, succ_cont)))

#define	debugtailcall(proc) \
	IF (calldebug, (save_transient_registers(), tailcall_msg(proc)))

#define	debugproceed() \
	IF (calldebug, (save_transient_registers(), proceed_msg()))

#define	debugmsg0(msg) \
	IF (progdebug, (printf(msg)))

#define	debugmsg1(msg, arg1) \
	IF (progdebug, (printf(msg, arg1)))

#define	debugmsg2(msg, arg1, arg2) \
	IF (progdebug, (printf(msg, arg1, arg2)))

#define	debugmsg3(msg, arg1, arg2, arg3) \
	IF (progdebug, (printf(msg, arg1, arg2, arg3)))

#endif

