/*
** Copyright (C) 1995-1997,2000-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_prof.h -- definitions for profiling.
** (See also mercury_heap_profiling.h and mercury_deep_profiling.h.)
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

extern MR_Code *	volatile	MR_prof_current_proc;

/*
** The following two macros are used to ensure that the profiler can
** use `MR_prof_current_proc' to determine what procedure is currently
** being executed when a profiling interrupt occurs.
*/

#ifdef MR_MPROF_PROFILE_TIME
  #define MR_set_prof_current_proc(target)		\
		(MR_prof_current_proc = (target))
  #define MR_update_prof_current_proc(target)		\
		(MR_prof_current_proc = (target))	
#else
  #define MR_set_prof_current_proc(target)		((void)0)
  #define MR_update_prof_current_proc(target)		((void)0)
#endif

/*
** This variable holds the address of the calling procedure
** for a call to mercury__do_call_closure or mercury__do_call_class_method.
*/

#ifdef MR_MPROF_PROFILE_CALLS
  extern MR_Code *	MR_prof_ho_caller_proc;
#endif
#ifdef MR_MPROF_PROFILE_CALLS
  #define MR_set_prof_ho_caller_proc(target) \
  		(MR_prof_ho_caller_proc = (target))
#else
  #define MR_set_prof_ho_caller_proc(target)	((void)0)
#endif

/*
** The MR_PROFILE() macro is used (by mercury_calls.h) to record each call.
*/

#ifdef	MR_MPROF_PROFILE_CALLS
  #define MR_PROFILE(callee, caller) MR_prof_call_profile((callee), (caller))
#else
  #define MR_PROFILE(callee, caller) ((void)0)
#endif

#ifdef MR_MPROF_PROFILE_CALLS
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
** to finish up profiling (writing the profiling data to files)
** at the end of the program, and to close the `Prof.Decl' file at end of
** module initialization.
** Note that prof_init() calls atexit(prof_finish), so that it can handle
** the case where the program exits by calling exit() rather than just
** returning, so it is actually not necessary to call prof_finish()
** explicitly.
*/

extern	void	MR_prof_init(void);
extern	void	MR_prof_finish(void);
extern	void	MR_close_prof_decl_file(void);

#ifdef MR_MPROF_PROFILE_TIME
  extern void 	MR_prof_turn_on_time_profiling(void);
  extern void	MR_prof_turn_off_time_profiling(void);
#endif

#endif	/* not MERCURY_PROF_H */
