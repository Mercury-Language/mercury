%---------------------------------------------------------------------------%
% Copyright (C) 1993-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module require.

% Main author: fjh.
% Stability: medium to high.

% This module provides features similar to <assert.h> in C.

%-----------------------------------------------------------------------------%
:- interface.

:- pred error(string).
:- mode error(in) is erroneous.

%	error(Message).
%		Abort with error message.


:- pred	require(pred, string).
:- mode	require((pred) is semidet, in) is det.

%	require(Goal, Message).
%		Call goal, and abort with error message if Goal fails.
%		This is not as useful as you might imagine, since it requires
%		that the goal not produce any output variables.  In
%		most circumstances you should use an explicit if-then-else
%		with a call to error/1 in the "else".

%-----------------------------------------------------------------------------%

:- implementation.

require(Goal, Message) :-
	( call(Goal) ->
		true
	;
		error(Message),
		fail
	).

%-----------------------------------------------------------------------------%

/* error/1, from require.m */

:- pragma c_header_code("
#include <stdio.h>
#include ""mercury_stack_trace.h""
").

% Hopefully error/1 won't be called often (!), so no point inlining it.
:- pragma no_inline(error/1). 

:- pragma c_code(error(Message::in), "
	fflush(stdout);
	fprintf(stderr, ""Software error: %s\\n"", Message);
	MR_trace_report(stderr);
	MR_dump_stack(MR_succip, MR_sp, MR_curfr);
	exit(1);
#ifndef USE_GCC_NONLOCAL_GOTOS
	return 0;	/* suppress some dumb warnings */
#endif
").

:- end_module require.

/*---------------------------------------------------------------------------*/
