/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
 * file: solutions.mod
 * author: conway.
 *
 * this module defines solutions/2 which takes a closure of type
 * pred(T) in which the remaining argument is output.
 */

#include "imp.h"

BEGIN_MODULE(solutions_module)

BEGIN_CODE

do_solutions:
	mkfame("solutions", 3, LABEL(no_more_solutions));

	framevar(0) = succip;

	framevar(1) = hp;

	framevar(2) = mkword(mktag(0),mkbody(0));

	r2 = (Word) 1;
	call_closure(more_solutions);

more_solutions:
	r3 = deep_copy(r2, framevar(1));
	framevar(2) = mkcons(r3, framevar(2));

	redo();

no_more_solutions:
	r2 = deep_recopy(framevar(2), framevar(1));
	maxfr = curprevfr;
	curfr = maxfr;

	succip = framevar(0);
	proceed();

END_MODULE
