%---------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Main author: conway
% Stability: medium.
%
% This module provides `spawn/3' which is the primitive for starting the
% concurrent execution of a goal. The term `concurrent' here is refering
% to threads, not parallel execution, though the latter is possible by
% compiling in the grade asm_fast.gc.par.
%
%---------------------------------------------------------------------------%
:- module spawn.

:- interface.

:- import_module io.

	% spawn(Closure, IO0, IO) is true iff IO0 denotes a list of I/O
	% transactions that is an interleaving of those performed by `Closure'
	% and those contained in IO - the list of transactions performed by
	% the continuation of spawn.
:- pred spawn(pred(io__state, io__state), io__state, io__state).
:- mode spawn(pred(di, uo) is cc_multi, di, uo) is cc_multi.

	% yield(IO0, IO) is logically equivalent to (IO = IO0) but
	% operationally, yields the mercury engine to some other thread
	% if one exists.
:- pred yield(io__state, io__state).
:- mode yield(di, uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- pragma c_header_code("
	#include <stdio.h>
").

:- pragma no_inline(spawn/3).
:- pragma c_code(spawn(Goal::(pred(di, uo) is cc_multi), IO0::di, IO::uo),
		will_not_call_mercury, "{
#ifndef MR_HIGHLEVEL_CODE
	MR_Context	*ctxt;
	ctxt = create_context();
	ctxt->resume = &&spawn_call_back_to_mercury_cc_multi;
		/* Store the closure on the top of the new context's stack. */
	*(ctxt->context_sp) = Goal;
	ctxt->next = NULL;
	schedule(ctxt);
	if (0) {
spawn_call_back_to_mercury_cc_multi:
		save_registers();
			/* Get the closure from the top of the stack */
		call_back_to_mercury_cc_multi(*((Word *)MR_sp));
		destroy_context(MR_ENGINE(this_context));
		runnext();
	}
	IO = IO0;
#else
	MR_fatal_error(""spawn is not implemented for highlevel code"");
#endif
}").

:- pragma no_inline(yield/2).
:- pragma c_code(yield(IO0::di, IO::uo),
		will_not_call_mercury, "{
		/* yield() */
	save_context(MR_ENGINE(this_context));
	MR_ENGINE(this_context)->resume = &&yield_skip_to_the_end;
	schedule(MR_ENGINE(this_context));
	runnext();
yield_skip_to_the_end:
	IO = IO0;
}").

:- pred call_back_to_mercury(pred(io__state, io__state), io__state, io__state).
:- mode call_back_to_mercury(pred(di, uo) is cc_multi, di, uo) is cc_multi.
:- pragma export(call_back_to_mercury(pred(di, uo) is cc_multi, di, uo),
		"call_back_to_mercury_cc_multi").

call_back_to_mercury(Goal) -->
	call(Goal).
