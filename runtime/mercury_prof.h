/*
** Copyright (C) 1995-1997,2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_prof.h -- definitions for profiling.
** (See also mercury_heap_profiling.h.)
*/

#ifndef MERCURY_PROF_H
#define MERCURY_PROF_H

#include "mercury_types.h"	/* for `MR_Code *' */

#include <stdio.h>

/*
** This variable holds the address of the "current" procedure so that
** when a profiling interrupt occurs, the profiler knows where we are,
** so that it can credit the time to the appropriate procedure.
*/

/*
** A pointer to the "Prof.Decl" file.
*/
#ifdef PROFILE_CALLS
  extern FILE	*decl_fptr;
#endif

/*
** The following two macros are used to ensure that the profiler can
** use `prof_current_proc' to determine what procedure is currently
** being executed when a profiling interrupt occurs.
*/

#define MR_set_prof_current_proc(target)	set_prof_current_proc(target)
#ifdef PROFILE_TIME
  #define set_prof_current_proc(target)		\
		(MR_prof_current_proc = (target))
  #define update_prof_current_proc(target)	\
		(MR_prof_current_proc = (target))	
#else
  #define set_prof_current_proc(target)		((void)0)
  #define update_prof_current_proc(target)	((void)0)
#endif

/*
** The PROFILE() macro is used (by mercury_calls.h) to record each call.
*/

#ifdef	PROFILE_CALLS
  #define PROFILE(callee, caller) MR_prof_call_profile((callee), (caller))
#else
  #define PROFILE(callee, caller) ((void)0)
#endif

#ifdef PROFILE_CALLS
  extern void	MR_prof_call_profile(MR_Code *, MR_Code *);
#endif


/*
** The prof_output_addr_decl() function is used by insert_entry() in
** mercury_label.c to record the address of each entry label.
*/

extern void	MR_prof_output_addr_decl(const char *name, const MR_Code *address);


/*
** The following functions are used by mercury_wrapper.c to
** initiate profiling, at the start of the the program,
** and to finish up profiling (writing the profiling data to files)
** at the end of the program.
** Note that prof_init() calls atexit(prof_finish), so that it can handle
** the case where the program exits by calling exit() rather than just
** returning, so it is actually not necessary to call prof_finish()
** explicitly.
*/

extern	void	MR_prof_init(void);
extern	void	MR_prof_finish(void);

#ifdef PROFILE_TIME
  extern void 	MR_prof_turn_on_time_profiling(void);
  extern void	MR_prof_turn_off_time_profiling(void);
#endif

#endif	/* not MERCURY_PROF_H */
