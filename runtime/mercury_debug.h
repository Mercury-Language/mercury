/*
** Copyright (C) 1995-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_debug.h - definitions for debugging messages */

#ifndef MERCURY_DEBUG_H
#define MERCURY_DEBUG_H

#include "mercury_types.h"		/* for MR_Word and MR_Code */
#include <stdio.h>			/* for FILE */

/*---------------------------------------------------------------------------*/

#ifdef MR_DEBUG_ON
	#define MR_DEBUG(X) X
#else
	#define MR_DEBUG(X)
#endif

#if !defined(MR_DEBUG_GOTOS)

#define	MR_debuggoto(label)			((void)0)
#define	MR_debugsreg()				((void)0)

#else

#define	MR_debuggoto(label) \
	(MR_assert(label), \
	MR_IF (MR_gotodebug, \
		(MR_save_transient_registers(), MR_goto_msg(label))))

#define	MR_debugsreg() \
	MR_IF (MR_sregdebug, \
		(MR_save_transient_registers(), MR_reg_msg()))

#endif

#ifndef MR_LOWLEVEL_DEBUG

#define	MR_debugcr1(val0, hp)			((void)0)
#define	MR_debugcr2(val0, val1, hp)		((void)0)
#define	MR_debugincrhp(val, hp)			((void)0)
#define	MR_debugincrsp(val, sp)			((void)0)
#define	MR_debugdecrsp(val, sp)			((void)0)
#define	MR_debugregs(msg)			((void)0)
#define	MR_debugframe(msg)			((void)0)
#define	MR_debugmkframe(predname)		((void)0)
#define	MR_debugsucceed()			((void)0)
#define	MR_debugsucceeddiscard()		((void)0)
#define	MR_debugfail()				((void)0)
#define	MR_debugredo()				((void)0)
#define	MR_debugcall(proc, succ_cont)		((void)0)
#define	MR_debugtailcall(proc)			((void)0)
#define	MR_debugproceed()			((void)0)
#define	MR_debugmsg0(msg)			((void)0)
#define	MR_debugmsg1(msg, arg1)			((void)0)
#define	MR_debugmsg2(msg, arg1, arg2)		((void)0)
#define	MR_debugmsg3(msg, arg1, arg2, arg3)	((void)0)

#else

#define	MR_debugcr1(val0, hp) \
	MR_IF (MR_heapdebug, \
		(MR_save_transient_registers(), MR_cr1_msg(val0, hp)))

#define	MR_debugcr2(val0, val1, hp) \
	MR_IF (MR_heapdebug, \
		(MR_save_transient_registers(), MR_cr2_msg(val0, val1, hp)))

#define	MR_debugincrhp(val, hp) \
	MR_IF (MR_heapdebug, \
		(MR_save_transient_registers(), MR_incr_hp_debug_msg((val), (hp))))

#define	MR_debugincrsp(val, sp) \
	MR_IF (MR_detstackdebug, \
		(MR_save_transient_registers(), MR_incr_sp_msg((val), (sp))))

#define	MR_debugdecrsp(val, sp) \
	MR_IF (MR_detstackdebug, \
		(MR_save_transient_registers(), MR_decr_sp_msg((val), (sp))))

#define	MR_debugregs(msg) \
	MR_IF (MR_progdebug, (MR_save_transient_registers(), MR_printregs(msg)))

#define	MR_debugmkframe(predname) \
	MR_IF (MR_nondstackdebug, \
		(MR_save_transient_registers(), MR_mkframe_msg(predname)))

#define	MR_debugframe(msg)	 \
	MR_IF (MR_progdebug, \
		(MR_save_transient_registers(), MR_printframe(msg)))

#define	MR_debugsucceed() \
	MR_IF (MR_calldebug, \
		(MR_save_transient_registers(), MR_succeed_msg()))

#define	MR_debugsucceeddiscard() \
	MR_IF (MR_calldebug, \
		(MR_save_transient_registers(), MR_succeeddiscard_msg()))

#define	MR_debugfail() \
	MR_IF (MR_calldebug, \
		(MR_save_transient_registers(), MR_fail_msg()))

#define	MR_debugredo() \
	MR_IF (MR_calldebug, \
		(MR_save_transient_registers(), MR_redo_msg()))

#define	MR_debugcall(proc, succ_cont) \
	MR_IF (MR_calldebug, \
		(MR_save_transient_registers(), MR_call_msg(proc, succ_cont)))

#define	MR_debugtailcall(proc) \
	MR_IF (MR_calldebug, \
		(MR_save_transient_registers(), MR_tailcall_msg(proc)))

#define	MR_debugproceed() \
	MR_IF (MR_calldebug, (MR_save_transient_registers(), MR_proceed_msg()))

#define	MR_debugmsg0(msg) \
	MR_IF (MR_progdebug, (printf(msg)))

#define	MR_debugmsg1(msg, arg1) \
	MR_IF (MR_progdebug, (printf(msg, arg1)))

#define	MR_debugmsg2(msg, arg1, arg2) \
	MR_IF (MR_progdebug, (printf(msg, arg1, arg2)))

#define	MR_debugmsg3(msg, arg1, arg2, arg3) \
	MR_IF (MR_progdebug, (printf(msg, arg1, arg2, arg3)))

#endif /* MR_LOWLEVEL_DEBUG */

#define	MR_print_deep_prof_vars(fp, msg)				\
	do {								\
		fprintf(fp, "%s\n", msg);				\
		MR_print_deep_prof_var(fp, "curcsd", 			\
			MR_current_call_site_dynamic);			\
		MR_print_deep_prof_var(fp, "nextcsd",			\
			MR_next_call_site_dynamic);			\
	} while (0)

/*---------------------------------------------------------------------------*/

#ifdef MR_LOWLEVEL_DEBUG
extern	void	MR_mkframe_msg(const char *);
extern	void	MR_succeed_msg(void);
extern	void	MR_succeeddiscard_msg(void);
extern	void	MR_fail_msg(void);
extern	void	MR_redo_msg(void);
extern	void	MR_call_msg(/* const */ MR_Code *proc,
			/* const */ MR_Code *succcont);
extern	void	MR_tailcall_msg(/* const */ MR_Code *proc);
extern	void	MR_proceed_msg(void);
extern	void	MR_cr1_msg(MR_Word val0, const MR_Word *addr);
extern	void	MR_cr2_msg(MR_Word val0, MR_Word val1, const MR_Word *addr);
extern	void	MR_incr_hp_debug_msg(MR_Word val, const MR_Word *addr);
extern	void	MR_incr_sp_msg(MR_Word val, const MR_Word *addr);
extern	void	MR_decr_sp_msg(MR_Word val, const MR_Word *addr);
#endif

#ifdef MR_DEBUG_GOTOS
extern	void	MR_goto_msg(/* const */ MR_Code *addr);
extern	void	MR_reg_msg(void);
#endif

#ifdef MR_LOWLEVEL_DEBUG
extern	void	MR_printint(MR_Word n);
extern	void	MR_printstring(const char *s);
extern	void	MR_printheap(const MR_Word *h);
extern	void	MR_dumpframe(/* const */ MR_Word *);
extern	void	MR_dumpnondstack(void);
extern	void	MR_printlist(MR_Word p);
extern	void	MR_printframe(const char *);
extern	void	MR_printregs(const char *msg);
#endif

extern	void	MR_printdetstack(const MR_Word *s);
extern	void	MR_printdetstackptr(const MR_Word *s);
extern	void	MR_print_detstackptr(FILE *fp, const MR_Word *s);
extern	void	MR_printnondstack(const MR_Word *s);
extern	void	MR_printnondstackptr(const MR_Word *s);
extern	void	MR_print_nondstackptr(FILE *fp, const MR_Word *s);
extern	void	MR_print_heapptr(FILE *fp, const MR_Word *s);
extern	void	MR_print_label(FILE *fp, /* const */ MR_Code *w);
extern	void	MR_printlabel(FILE *fp, /* const */ MR_Code *w);
extern	void	MR_print_deep_prof_var(FILE *fp, const char *name,
			MR_CallSiteDynamic *csd);

/*---------------------------------------------------------------------------*/

#endif /* not MERCURY_DEBUG_H */
