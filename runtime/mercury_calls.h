/*
** Copyright (C) 1995-1997 The University of Melbourne.
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

#define	noprof_localcall(label, succ_cont)			\
		do {						\
			debugcall(LABEL(label), (succ_cont));	\
			MR_succip = (succ_cont);			\
			set_prof_current_proc(LABEL(label));	\
			GOTO_LABEL(label);			\
		} while (0)

/*
** On some systems [basically those using PIC (Position Independent Code)],
** if we're using gcc non-local gotos to jump between functions then
** we need to do ASM_FIXUP_REGS after each return from a procedure call.
**
** We *don't* need to do this if we are using NATIVE_GC, because all
** labels are defined as entry labels anyway. Entry labels do 
** ASM_FIXUP_REGS immediately. Also, for NATIVE_GC we need the succip
** set to the address of the continuation label, not the fixup_gp:
** label.
*/
#if defined(USE_GCC_NONLOCAL_GOTOS) && defined(NEED_ASM_FIXUP_REGS) &&	\
	!defined(NATIVE_GC)
  #define	noprof_call(proc, succ_cont)			\
		({						\
			__label__ fixup_gp;			\
			debugcall((proc), (succ_cont));		\
			MR_succip = (&&fixup_gp);		\
			set_prof_current_proc(proc);		\
			GOTO(proc);				\
		fixup_gp:					\
			ASM_FIXUP_REGS				\
			GOTO(succ_cont); 			\
		})
	/* same as above, but with GOTO_LABEL rather than GOTO */
  #define	noprof_call_localret(proc, succ_cont)		\
		({						\
			__label__ fixup_gp;			\
			debugcall((proc), (succ_cont));		\
			MR_succip = (&&fixup_gp);		\
			set_prof_current_proc(proc);		\
			GOTO(proc);				\
		fixup_gp:					\
			ASM_FIXUP_REGS				\
			GOTO_LABEL(succ_cont); 			\
		})
#else
  #define	noprof_call(proc, succ_cont)			\
		do {						\
			debugcall((proc), (succ_cont));		\
			MR_succip = (succ_cont);		\
			set_prof_current_proc(proc);		\
			GOTO(proc);				\
		} while (0)
  #define noprof_call_localret(proc, succ_cont) 		\
		noprof_call((proc), LABEL(succ_cont))
#endif

#define	localcall(label, succ_cont, current_label)		\
		do {						\
			debugcall(LABEL(label), (succ_cont));	\
			MR_succip = (succ_cont);		\
			PROFILE(LABEL(label), (current_label));	\
			set_prof_current_proc(LABEL(label));	\
			GOTO_LABEL(label);			\
		} while (0)

#define	call(proc, succ_cont, current_label)			\
		do {						\
			PROFILE((proc), (current_label));	\
			noprof_call((proc), (succ_cont));	\
		} while (0)

#define	call_localret(proc, succ_cont, current_label)		\
		do {						\
			PROFILE((proc), (current_label));	\
			noprof_call_localret(proc, succ_cont);	\
		} while (0)

#define	call_det_closure(succ_cont, current_label)		\
		do {						\
			Declare_entry(do_call_det_closure);	\
			call(ENTRY(do_call_det_closure),	\
				(succ_cont), (current_label));	\
		} while (0)

#define	call_semidet_closure(succ_cont, current_label)		\
		do {						\
			Declare_entry(do_call_semidet_closure); \
			call(ENTRY(do_call_semidet_closure),	\
				(succ_cont), (current_label));	\
		} while (0)

#define	call_nondet_closure(succ_cont, current_label)		\
		do {						\
			Declare_entry(do_call_nondet_closure);	\
			call(ENTRY(do_call_nondet_closure),	\
				(succ_cont), (current_label));	\
		} while (0)

#define	localtailcall(label, current_label)			\
		do {						\
			debugtailcall(LABEL(label));		\
			PROFILE(LABEL(label), (current_label)); \
			set_prof_current_proc(LABEL(label));	\
			GOTO_LABEL(label);			\
		} while (0)

#define	tailcall(proc, current_label)				\
		do {						\
			debugtailcall(proc);			\
			PROFILE((proc), (current_label));	\
			set_prof_current_proc(proc);		\
			GOTO(proc);				\
		} while (0)

#define	proceed()						\
		do {						\
			debugproceed();				\
			GOTO(MR_succip);			\
		} while (0)

#endif /* not MERCURY_CALLS_H */
