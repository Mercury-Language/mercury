/*
**      Definitions for the profiling module
**
**	Main Author : petdr
*/

#ifndef PROF_H
#define PROF_H

#include        "imp.h"

/*******************
  Need to make this a command line option
*******************/
#define PROF_TABLE_SIZE 4096


/* profiling node information */
typedef struct s_prof_node
{
        Code *Callee, *Caller;
        unsigned long count;
        struct s_prof_node *next;
} prof_node;


/* Macro definitions */
#define hash_addr_pair(Callee, Caller) \
        (int) ((( (unsigned long)(Callee) ^ (unsigned long)(Caller) ) >> 2) \
		% PROF_TABLE_SIZE )


/* function prototypes */                                    
extern  void    prof_call_profile(Code *, Code *);
extern	void	prof_output_addr_pair_table(void);
extern	void	prof_output_addr_decls(const char *, const Code *);
#endif
