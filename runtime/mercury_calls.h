/*
** Copyright (C) 1995-2000, 2004 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_calls.h - definitions for calls and returns */

#ifndef MERCURY_CALLS_H
#define MERCURY_CALLS_H

#include "mercury_regs.h"	/* for MR_succip */
#include "mercury_goto.h"	/* calls are implemented using gotos */
#include "mercury_debug.h"	/* we need to debug them */
#include "mercury_prof.h"	/* we need to profile them */

#define	MR_noprof_localcall(label, succ_cont)			\
		do {						\
			MR_debugcall(MR_LABEL(label), (succ_cont));\
			MR_succip = (succ_cont);		\
			MR_GOTO_LABEL(label);			\
		} while (0)

/*
** On some systems [basically those using PIC (Position Independent MR_Code)],
** if we're using gcc non-local gotos to jump between functions then
** we need to do ASM_FIXUP_REGS after each return from a procedure call.
** However, if we're using asm labels, then this is done in the
** MR_define_label(), MR_define_static(), and MR_define_Entry() macros,
** so there's no need to do it here.
** Also if we're using native gc, then the fixup_gp label below
** would stuff up the succip values, so we can't do it.
**
** For the non-asm `jump' and `fast' grades, we do in theory need
** to do something like this.  However, doing ASM_FIXUP_REGS only
** on returns is *not* sufficient to ensure correctness in general.
** So the non-asm `jump' and `fast' grades are in theory broken,
** with or without this code.  In practice they happen to work
** (for our current set of test cases!) either way.
** So it is simpler (and more efficient) to just comment it out.
*/
#if 0
  #define	MR_noprof_call(proc, succ_cont)			\
		({						\
			__label__ fixup_gp;			\
			MR_debugcall((proc), (succ_cont));	\
			MR_succip = (&&fixup_gp);		\
			MR_GOTO(proc);				\
		fixup_gp:					\
			ASM_FIXUP_REGS				\
			MR_GOTO(succ_cont); 			\
		})
	/* same as above, but with MR_GOTO_LABEL rather than MR_GOTO */
  #define	MR_noprof_call_localret(proc, succ_cont)	\
		({						\
			__label__ fixup_gp;			\
			MR_debugcall((proc), (succ_cont));	\
			MR_succip = (&&fixup_gp);		\
			MR_GOTO(proc);				\
		fixup_gp:					\
			ASM_FIXUP_REGS				\
			MR_GOTO_LABEL(succ_cont); 		\
		})
#else
  #define	MR_noprof_call(proc, succ_cont)			\
		do {						\
			MR_debugcall((proc), (succ_cont));	\
			MR_succip = (succ_cont);		\
			MR_GOTO(proc);				\
		} while (0)
  #define 	MR_noprof_call_localret(proc, succ_cont) 	\
		MR_noprof_call((proc), MR_LABEL(succ_cont))
#endif

#define	MR_localcall(label, succ_cont, current_label)		\
		do {						\
			MR_debugcall(MR_LABEL(label), (succ_cont)); \
			MR_succip = (succ_cont);		\
			MR_PROFILE(MR_LABEL(label), (current_label)); \
			MR_set_prof_current_proc(MR_LABEL(label)); \
			MR_GOTO_LABEL(label);			\
		} while (0)

#define	MR_call(proc, succ_cont, current_label)			\
		do {						\
			MR_PROFILE((proc), (current_label));	\
			MR_set_prof_current_proc(proc);		\
			MR_noprof_call((proc), (succ_cont));	\
		} while (0)

#define	MR_call_localret(proc, succ_cont, current_label)	\
		do {						\
			MR_PROFILE((proc), (current_label));	\
			MR_set_prof_current_proc(proc);		\
			MR_noprof_call_localret(proc, succ_cont);\
		} while (0)

#define	MR_localtailcall(label, current_label)			\
		do {						\
			MR_debugtailcall(MR_LABEL(label));	\
			MR_PROFILE(MR_LABEL(label), (current_label)); \
			MR_set_prof_current_proc(MR_LABEL(label)); \
			MR_GOTO_LABEL(label);			\
		} while (0)

#define	MR_noprof_localtailcall(label)				\
		do {						\
			MR_debugtailcall(MR_LABEL(label));	\
			MR_GOTO_LABEL(label);			\
		} while (0)

#define	MR_tailcall(proc, current_label)			\
		do {						\
			MR_debugtailcall(proc);			\
			MR_PROFILE((proc), (current_label));	\
			MR_set_prof_current_proc(proc);		\
			MR_GOTO(proc);				\
		} while (0)

#define	MR_noprof_tailcall(proc)				\
		do {						\
			MR_debugtailcall(proc);			\
			MR_GOTO(proc);				\
		} while (0)

#define	MR_proceed()						\
		do {						\
			MR_debugproceed();			\
			MR_GOTO(MR_succip);			\
		} while (0)

#endif /* not MERCURY_CALLS_H */
