/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
 * file: solutions.mod
 * authors: conway, fjh.
 *
 * this module defines solutions/2 which takes a closure of type
 * pred(T) in which the remaining argument is output.
 */

#include "imp.h"

BEGIN_MODULE(solutions_module)

BEGIN_CODE

/*
** The following is an incomplete start at implementing solutions/2
** for gc != conservative.
**
**
**	do_solutions:
**		mkframe("solutions", 3, LABEL(no_more_solutions));
**	
**		framevar(0) = succip;
**	
**		framevar(1) = hp;
**	
**		framevar(2) = list_empty();
**	
**		r2 = (Word) 1;
**		call(ENTRY(do_call_nondet_closure),
**			LABEL(more_solutions), LABEL(do_solutions));
**	
**	more_solutions:
**		r3 = deep_copy(r1, framevar(1));
**		framevar(2) = list_cons(r3, framevar(2));
**	
**		redo();
**	
**	no_more_solutions:
**		r2 = deep_recopy(framevar(2), framevar(1));
**		maxfr = curprevfr;
**		curfr = maxfr;
**	
**		succip = framevar(0);
**		proceed();
*/

/*
:- pred builtin_solutions(pred(T), list(T)).
:- mode builtin_solutions(complicated_mode, out) is det.
	% r1 - typeinfo for T (unused)
	% r2 - closure
	% r3 - output list
*/

mercury__std_util__builtin_solutions_2_0:
mercury__std_util__builtin_solutions_2_1:

/*
** The following algorithm is very straight-forward implementation
** but only works with `--gc conservative'.
** Since with conservative gc, we don't reclaim any memory on failure,
** but instead leave it to the garbage collector, there is no need to
** make deep copies of the solutions.  This is a `copy-zero' implementation ;-)
*/

#ifndef CONSERVATIVE_GC
	fatal_error("solutions/2 only implemented for conservative GC");
#endif

	/* create a nondet stack frame with one slot, to hold the list
	   of solutions, and set the failure continuation */
	mkframe("builtin_solutions", 1,
		LABEL(mercury__std_util__builtin_solutions_2_0_i2));
	framevar(0) = list_empty();

	/* call the higher-order pred closure that we were passed in r2 */
	r1 = r2;
	r2 = (Word) 0;	/* the closure has no input arguments */
	r3 = (Word) 1;	/* the closure has one argument */
	{ 
		Declare_entry(do_call_nondet_closure);
		call(ENTRY(do_call_nondet_closure),
			LABEL(mercury__std_util__builtin_solutions_2_0_i1),
			LABEL(mercury__std_util__builtin_solutions_2_0));
	}

mercury__std_util__builtin_solutions_2_0_i1:
	/* we found a solution */
	/* insert it into the list, and then look for the next one */
	framevar(0) = list_cons(r1, framevar(0));
	redo();

mercury__std_util__builtin_solutions_2_0_i2:
	/* no more solutions */
	/* put the list in r3, discard the frame we made, and return */
	r3 = framevar(0);
	succeed_discard();

END_MODULE
