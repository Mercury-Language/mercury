/*
** Copyright (C) 1995-1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* prof.h -- definitions for profiling */

#ifndef PROF_H
#define PROF_H

#include "mercury_types.h"	/* for `Code *' */

/*
** this variable holds the address of the "current" procedure so that
** when a profiling interrupt occurs, the profiler knows where we are,
** so that it can credit the time to the appropriate procedure.
*/

extern	Code *	volatile	prof_current_proc;

/*
** the following two macros are used to ensure that the profiler can
** use `prof_current_proc' to determine what procedure is currently
** being executed when a profiling interrupt occurs
*/

#ifdef PROFILE_TIME
#define set_prof_current_proc(target)		(prof_current_proc = (target))
#define update_prof_current_proc(target)	(prof_current_proc = (target))	
#else
#define set_prof_current_proc(target)		((void)0)
#define update_prof_current_proc(target)	((void)0)
#endif

#ifdef	PROFILE_CALLS
#define	PROFILE(callee, caller)		prof_call_profile((callee), (caller))
#else
#define	PROFILE(callee, caller)		((void)0)
#endif

extern	void 	prof_init_time_profile(void);
extern  void    prof_call_profile(Code *, Code *);
extern	void	prof_turn_off_time_profiling(void);
extern	void	prof_output_addr_pair_table(void);
extern	void	prof_output_addr_decls(const char *, const Code *);
extern	void	prof_output_addr_table(void);

#endif	/* not PROF_H */
