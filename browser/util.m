%---------------------------------------------------------------------------%
% Copyright (C) 1998-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module mdb__util.

:- interface.

:- import_module list, string, io.

% The stuff defined below is similar to types goal_path and trace_port
% defined in modules compiler/hlds_goal.m and compiler/trace.m.
% This enumeration must be EXACTLY the same as the MR_trace_port enum in
% runtime/mercury_trace_base.h, and in the same order, since the code
% assumes the representation is the same.

:- type trace_port_type
	--->	call
	;	exit
	;	redo
	;	fail
	;	exception
	;	ite_cond
	;	ite_then
	;	ite_else
	;	neg_enter
	;	neg_success
	;	neg_failure
	;	disj
	;	switch
	;	nondet_pragma_first
	;	nondet_pragma_later
	.

% This enumeration must be EXACTLY the same as the MR_PredFunc enum in
% runtime/mercury_stack_layout.h, and in the same order, since the code
% assumes the representation is the same.

:- type pred_or_func
	--->	predicate
	;	function.

:- type goal_path_string == string.

:- type line_number == int.

	% Get user input via the same method used by the internal
	% debugger.
:- pred util__trace_getline(string, io__result(string), io__state,
		io__state).
:- mode util__trace_getline(in, out, di, uo) is det.

:- pred util__trace_getline(string, io__result(string), io__input_stream,
		io__output_stream, io__state, io__state).
:- mode util__trace_getline(in, out, in, in, di, uo) is det.

	% trace_get_command is similar to trace_getline except that it
	% breaks lines into semicolon separated commands, and replaces
	% EOF with the command 'quit'.
:- pred util__trace_get_command(string, string, io__state, io__state).
:- mode util__trace_get_command(in, out, di, uo) is det.

:- pred util__trace_get_command(string, string, io__input_stream,
		io__output_stream, io__state, io__state).
:- mode util__trace_get_command(in, out, in, in, di, uo) is det.

:- pred util__zip_with(pred(T1, T2, T3), list(T1), list(T2), list(T3)).
:- mode util__zip_with(pred(in, in, out) is det, in, in, out) is det.

	% Apply predicate to argument repeatedly until the result
	% remains the same.
:- pred util__limit(pred(list(T), list(T)), list(T), list(T)).
:- mode util__limit(pred(in,out) is det, in, out) is det.

	% For use in representing unbound head variables in the "print goal"
	% commands in the debugger.
:- type unbound ---> '_'.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module bool, int, require.
:- pragma foreign_import_module(c, bool).

util__trace_getline(Prompt, Result) -->
	io__input_stream(MdbIn),
	io__output_stream(MdbOut),
	util__trace_getline(Prompt, Result, MdbIn, MdbOut).

util__trace_getline(Prompt, Result, MdbIn, MdbOut) -->
	call_trace_getline(MdbIn, MdbOut, Prompt, Line, Success),
	{
		Success = yes,
		Result = ok(Line)
	;
		Success = no,
		Result = eof
	}.

:- pred call_trace_getline(input_stream::in, output_stream::in, string::in,
	string::out, bool::out, io__state::di, io__state::uo) is det.

:- pragma c_header_code("
	#include ""mercury_wrapper.h""
	#include ""mercury_string.h""
	#include ""mercury_trace_base.h""
	#include ""mercury_trace_internal.h""
	#include ""mercury_library_types.h""
").

:- pragma foreign_proc("C",
	call_trace_getline(MdbIn::in, MdbOut::in, Prompt::in, Line::out,
		Success::out, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, tabled_for_io],
"
	char		*line;
	MercuryFile	*mdb_in = (MercuryFile *) MdbIn;
	MercuryFile	*mdb_out = (MercuryFile *) MdbOut;

	if (MR_address_of_trace_getline != NULL) {
		line = (*MR_address_of_trace_getline)((char *) Prompt,
				MR_file(*mdb_in), MR_file(*mdb_out));
	} else {
		MR_tracing_not_enabled();
		/* not reached */
	}

	if (line == NULL) {
		/* we copy the null string to avoid warnings about const */
		MR_make_aligned_string_copy(Line, """");
		Success = ML_bool_return_no();
	} else {
		MR_make_aligned_string_copy(Line, line);
		MR_free(line);
		Success = ML_bool_return_yes();
	}
").

util__trace_get_command(Prompt, Result) -->
	io__input_stream(MdbIn),
	io__output_stream(MdbOut),
	util__trace_get_command(Prompt, Result, MdbIn, MdbOut).

:- pragma foreign_proc("C",
	util__trace_get_command(Prompt::in, Line::out, MdbIn::in,
		MdbOut::in, State0::di, State::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	char		*line;
	MercuryFile	*mdb_in = (MercuryFile *) MdbIn;
	MercuryFile	*mdb_out = (MercuryFile *) MdbOut;

	if (MR_address_of_trace_getline != NULL) {
		line = (*MR_address_of_trace_get_command)(
				(char *) Prompt,
				MR_file(*mdb_in), MR_file(*mdb_out));
	} else {
		MR_tracing_not_enabled();
		/* not reached */
	}

	MR_make_aligned_string_copy(Line, line);
	MR_free(line);

	State = State0;
").

util__zip_with(Pred, XXs, YYs, Zipped) :-
	( (XXs = [], YYs = []) ->
		Zipped = []
	; (XXs = [X|Xs], YYs = [Y|Ys]) ->
		Pred(X,Y,PXY),
		Zipped = [PXY|Rest],
		util__zip_with(Pred, Xs, Ys, Rest)
	;
		error("zip_with: list arguments are of unequal length")
	).

util__limit(Pred, Xs, Ys) :-
	Pred(Xs, Zs),
	( Xs = Zs ->
		Ys = Zs
	;
		util__limit(Pred, Zs, Ys)
	).

%---------------------------------------------------------------------------%
