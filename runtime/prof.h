/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
**      Definitions for the profiling module
**
**	Main Author : petdr
*/

#ifndef PROF_H
#define PROF_H

#include        "imp.h"

/* this variable holds the address of the "current" procedure so that
   when a profiling interrupt occurs, the profiler knows where we are,
   so that it can credit the time to the appropriate procedure */
extern Code *prof_current_proc;

/* function prototypes */                                    
extern	void 	prof_init_time_profile(void);
extern  void    prof_call_profile(Code *, Code *);
extern	void	prof_time_profile(int);
extern	void	prof_turn_off_time_profiling(void);
extern	void	prof_output_addr_pair_table(void);
extern	void	prof_output_addr_decls(const char *, const Code *);
extern	void	prof_output_addr_table(void);
#endif
