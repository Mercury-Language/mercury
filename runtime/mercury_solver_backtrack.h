/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_solver_backtrack.h - macros etc. for interfacing with the CLP(R)
** 	solver. To interface with a different solver, the macros etc. in 
**	this file should be updated accordingly.
**	This header file is included when using a *.cnstr grade. 
**	NOTE: You will have to link the CLP(R) object files in by hand,
**	eg. by using MLFLAGS=<pathname>/clpr/ *.o 
**	(the ' ' before the '*'	shouldn't be there, but otherwise it would 
**	be the start of a comment...)
*/

#ifndef MERCURY_SOLVER_BACKTRACK_H
#define MERCURY_SOLVER_BACKTRACK_H

	/* Mercury headers */
#include "imp.h"
#include "misc.h"

	/* CLP(R) headers */
#include "clpr/clpr/emul.h"
#include "clpr/clpr/config.h"

#define SOLVER_STACK_SIZE 10	/* default to 10 kb for the ticket stack */

	/* some Mercury variables to keep track of the ticket stack */
extern int *mercury_solver_sp;
extern int *mercury_solver_sp_old;
extern int solver_ticket_stack_size;

	/* some CLP(R) internals */
extern int slack_id;
extern int solver_id;
extern int trtop;
extern int **trail;
extern int stamp;

	/* CLP(R) streams - these might need to be */
	/* used if the solver gets into trouble    */
extern FILE *error_stream;
extern FILE *outfile;

	/* some CLP(R) functions that are called from here */
extern void init_solver(void);
extern void init_solver_goal(void);
void solver_backtrack(int);

	/* a CLP(R) function - used if there is a solver error */
extern void fatal(const char *);

/*
** The following macros define how to store and retreive a 'ticket' - the 
** information the solver needs to be able to backtrack. 
** In the case of CLP(R), we must save the stamp, slack_id, solver_id, and 
** trtop.
*/

	/* store a ticket in the stack frame starting at x. */
	/* XXX We abort if stack overflow occurs. We should */
	/* XXX instead dynamically re-allocate a new stack  */
	/* XXX Note also that we assume that 'stamp'        */
	/* XXX doesn't wrap around here, but that is an     */
	/* XXX assumption of the CLP(R) internals.          */
#define store_ticket(x)					\
	do {						\
	if(no_solver_stack_overflow()) {		\
		(x) = (Integer)mercury_solver_sp;	\
		*(mercury_solver_sp++) = ++stamp;	\
		*(mercury_solver_sp++) = slack_id;	\
		*(mercury_solver_sp++) = solver_id;	\
		*(mercury_solver_sp++) = trtop;		\
		} 					\
	else {						\
		fatal_error("Solver stack overflow");	\
		}					\
	} while(0)

	/* restore the solver to the state given in the ticket starting at x */
#define restore_ticket(x)				\
	do {						\
	mercury_solver_sp = (int *)(x);			\
	stamp = *(mercury_solver_sp++);			\
	slack_id = *(mercury_solver_sp++);		\
	solver_id = *(mercury_solver_sp++);		\
		{					\
		int old_trtop;				\
		old_trtop= *(mercury_solver_sp++);	\
		save_transient_registers();		\
		mercury_solver_untrail_to(old_trtop);	\
		restore_transient_registers();		\
		trtop = old_trtop;			\
		}					\
	} while(0)

	/* discard the top ticket */
	/* Note: the stack pointer is decremented by 4 because */
	/* each stack frame contains 4 values                  */
#define discard_ticket()	(mercury_solver_sp -= 4)

	/* trail the solver */
	/* XXX note that the cast to int in the following      */
	/* XXX macro seems to assume that                      */
	/* XXX sizeof(int) == sizeof(int *), which is a little */
	/* XXX non-portable, but this is part of the CLP(R)    */
	/* XXX internals.                                      */
#define mercury_solver_untrail_to(new_trtop)		\
	do {						\
	int i;						\
	for (i = trtop - 1; i >= (new_trtop); i--) 	\
		solver_backtrack((int)trail[i]);	\
	} while(0)

	/* check for solver stack overflow*/
#define no_solver_stack_overflow()			\
	(mercury_solver_sp < 				\
		mercury_solver_sp_old + solver_ticket_stack_size )


	/* This macro performs the necessary initialisations */
	/* for the solver				     */
#define perform_solver_initialisations()		\
	do {						\
		init_solver();				\
		init_solver_goal();			\
			/* get some memory for */	\
			/* the CLP(R) trail    */	\
		trail = checked_malloc(DEF_TRAIL_SZ*sizeof(int)); \
		trtop = 0;				\
			/* initialise the      */	\
			/* CLP(R) streams      */	\
		error_stream = stderr;			\
		outfile = stderr;			\
	} while (0)

#endif
