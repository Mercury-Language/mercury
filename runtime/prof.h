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

/*******************
  Need to make these command line options
*******************/
#define CALL_TABLE_SIZE 4096
#define TIME_TABLE_SIZE 4096
#define	CLOCK_TICKS	1

#define	USEC		1000000

/* this variable holds the address of the "current" procedure so that
   when a profiling interrupt occurs, the profiler knows where we are,
   so that it can credit the time to the appropriate procedure */
extern Code *prof_current_proc;

/* profiling node information */
typedef struct s_prof_call_node
{
        Code *Callee, *Caller;
        unsigned long count;
        struct s_prof_call_node *next;
} prof_call_node;

typedef struct s_prof_time_node
{
	Code *Addr;
	unsigned long count;
	struct s_prof_time_node *next;
} prof_time_node;


/* Macro definitions */
#define hash_addr_pair(Callee, Caller)                                      \
        (int) ((( (unsigned long)(Callee) ^ (unsigned long)(Caller) ) >> 2) \
		% CALL_TABLE_SIZE )

#define hash_prof_addr(Addr)   						    \
	(int) ( (unsigned long)(Addr) % TIME_TABLE_SIZE )


/* function prototypes */                                    
extern	void 	prof_init_time_profile(void);
extern  void    prof_call_profile(Code *, Code *);
extern	void	prof_time_profile(int);
extern	void	prof_turn_off_time_profiling(void);
extern	void	prof_output_addr_pair_table(void);
extern	void	prof_output_addr_decls(const char *, const Code *);
extern	void	prof_output_addr_table(void);
#endif
