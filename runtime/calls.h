/*
** Copyright (C) 1995-1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* calls.h - definitions for calls and returns */

#ifndef CALLS_H
#define CALLS_H

#include "regs.h"	/* for succip */
#include "goto.h"	/* calls are implemented using gotos */
#include "debug.h"	/* we need to debug them */
#include "prof.h"	/* we need to profile them */

#define	noprof_localcall(label, succ_cont)			\
		do {						\
			debugcall(LABEL(label), (succ_cont));	\
			succip = (succ_cont);			\
			set_prof_current_proc(LABEL(label));	\
			GOTO_LABEL(label);			\
		} while (0)

/*
** On some systems [basically those using PIC (Position Independent Code)],
** if we're using gcc non-local gotos to jump between functions then
** we need to do ASM_FIXUP_REGS after each return from a procedure call.
*/
#if defined(USE_GCC_NONLOCAL_GOTOS) && defined(NEED_ASM_FIXUP_REGS)
  #define	noprof_call(proc, succ_cont)			\
		({						\
			__label__ fixup_gp;			\
			debugcall((proc), (succ_cont));		\
			succip = (&&fixup_gp);			\
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
			succip = (&&fixup_gp);			\
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
			succip = (succ_cont);			\
			set_prof_current_proc(proc);		\
			GOTO(proc);				\
		} while (0)
  #define noprof_call_localret(proc, succ_cont) 		\
		noprof_call((proc), LABEL(succ_cont))
#endif

#define	localcall(label, succ_cont, current_label)		\
		do {						\
			debugcall(LABEL(label), (succ_cont));	\
			succip = (succ_cont);			\
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
			noprof_call_localret(proc, succ_cont); \
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
			GOTO(succip);				\
		} while (0)

#endif /* not CALLS_H */
