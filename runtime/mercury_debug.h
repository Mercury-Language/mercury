/*
** Copyright (C) 1995-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_debug.h - definitions for debugging messages */

#ifndef MERCURY_DEBUG_H
#define MERCURY_DEBUG_H

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
	IF (gotodebug, (save_transient_registers(), goto_debug(label))))

#define	debugsreg() \
	IF (sregdebug, (save_transient_registers(), reg_debug()))

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
	IF (heapdebug, (save_transient_registers(), cr1_debug(val0, hp)))

#define	debugcr2(val0, val1, hp) \
	IF (heapdebug, (save_transient_registers(), cr2_debug(val0, val1, hp)))

#define	debugincrhp(val, hp) \
	IF (heapdebug, \
		(save_transient_registers(), incr_hp_debug_msg((val), (hp))))

#define	debugincrsp(val, sp) \
	IF (detstackdebug, (save_transient_registers(), incr_sp_debug((val), (sp))))

#define	debugdecrsp(val, sp) \
	IF (detstackdebug, (save_transient_registers(), decr_sp_debug((val), (sp))))

#define	debugpush(val, sp) \
	IF (detstackdebug, (save_transient_registers(), push_debug((val), (sp))))

#define	debugpop(val, sp) \
	IF (detstackdebug, (save_transient_registers(), pop_debug(val, sp)))

#define	debugregs(msg) \
	IF (progdebug, (save_transient_registers(), printregs(msg)))

#define	debugmkframe() \
	IF (nondstackdebug, (save_transient_registers(), mkframe_debug()))

#define	debugframe(msg)	 \
	IF (progdebug, (save_transient_registers(), printframe(msg)))

#define	debugmodframe() \
	IF (nondstackdebug, (save_transient_registers(), modframe_debug()))

#define	debugsucceed() \
	IF (nondstackdebug, (save_transient_registers(), succeed_debug()))

#define	debugsucceeddiscard() \
	IF (nondstackdebug, (save_transient_registers(), succeeddiscard_debug()))

#define	debugfail() \
	IF (nondstackdebug, (save_transient_registers(), fail_debug()))

#define	debugredo() \
	IF (nondstackdebug, (save_transient_registers(), redo_debug()))

#define	debugcall(proc, succ_cont) \
	IF (calldebug, (save_transient_registers(), call_debug(proc, succ_cont)))

#define	debugtailcall(proc) \
	IF (calldebug, (save_transient_registers(), tailcall_debug(proc)))

#define	debugproceed() \
	IF (calldebug, (save_transient_registers(), proceed_debug()))

#define	debugmsg0(msg) \
	IF (progdebug, (printf(msg)))

#define	debugmsg1(msg, arg1) \
	IF (progdebug, (printf(msg, arg1)))

#define	debugmsg2(msg, arg1, arg2) \
	IF (progdebug, (printf(msg, arg1, arg2)))

#define	debugmsg3(msg, arg1, arg2, arg3) \
	IF (progdebug, (printf(msg, arg1, arg2, arg3)))

#endif /* not MERCURY_DEBUG_H */

