%---------------------------------------------------------------------------%
% Copyright (C) 1998-2002, 2004-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module mdb.util.

:- interface.

:- import_module mdbcomp.prim_data.

:- import_module list, string, io, bool.

:- func util__is_predicate(pred_or_func) = bool.
:- func util__is_function(pred_or_func) = bool.

% This is similar to the type goal_path defined in the module
% compiler/hlds_goal.m.

:- type goal_path_string == string.

:- type line_number == int.

	% Get user input via the same method used by the internal
	% debugger.
:- pred util__trace_getline(string::in, io__result(string)::out,
	io::di, io::uo) is det.

:- pred util__trace_getline(string::in, io__result(string)::out,
	io__input_stream::in, io__output_stream::in, io::di, io::uo) is det.

	% trace_get_command is similar to trace_getline except that it
	% breaks lines into semicolon separated commands, and replaces
	% EOF with the command 'quit'.
:- pred util__trace_get_command(string::in, string::out, io::di, io::uo)
	is det.

:- pred util__trace_get_command(string::in, string::out,
	io__input_stream::in, io__output_stream::in, io::di, io::uo) is det.

:- pred util__zip_with(pred(T1, T2, T3)::in(pred(in, in, out) is det),
	list(T1)::in, list(T2)::in, list(T3)::out) is det.

	% Apply predicate to argument repeatedly until the result
	% remains the same.
:- pred util__limit(pred(list(T), list(T))::in(pred(in, out) is det),
	list(T)::in, list(T)::out) is det.

	% For use in representing unbound head variables in the "print goal"
	% commands in the debugger.
:- type unbound ---> '_'.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int, require.

util__is_predicate(predicate) = yes.
util__is_predicate(function) = no.

util__is_function(predicate) = no.
util__is_function(function) = yes.

util__trace_getline(Prompt, Result, !IO) :-
	io__input_stream(MdbIn, !IO),
	io__output_stream(MdbOut, !IO),
	util__trace_getline(Prompt, Result, MdbIn, MdbOut, !IO).

util__trace_getline(Prompt, Result, MdbIn, MdbOut, !IO) :-
	call_trace_getline(MdbIn, MdbOut, Prompt, Line, Success, !IO),
	( Success \= 0 ->
		Result = ok(Line)
	;
		Result = eof
	).

:- pred call_trace_getline(input_stream::in, output_stream::in, string::in,
	string::out, int::out, io__state::di, io__state::uo) is det.

:- pragma foreign_decl("C", "
	#include ""mercury_wrapper.h""
	#include ""mercury_string.h""
	#include ""mercury_trace_base.h""
	#include ""mercury_trace_internal.h""
	#include ""mercury_library_types.h""
").

:- pragma foreign_proc("C",
	call_trace_getline(MdbIn::in, MdbOut::in, Prompt::in, Line::out,
		Success::out, IO0::di, IO::uo),
	% We need to use will_not_call_mercury here,
	% because MR_make_aligned_string_copy() references MR_hp,
	% which only works for will_not_call_mercury foreign_procs.
	[will_not_call_mercury, promise_pure, tabled_for_io],
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
		Success = 0;
	} else {
		MR_make_aligned_string_copy(Line, line);
		MR_free(line);
		Success = 1;
	}

	IO = IO0;
").

call_trace_getline(_, _, _, _, _, !IO) :-
	private_builtin__sorry("mdb__util__call_trace_getline").

util__trace_get_command(Prompt, Result, !IO) :-
	io__input_stream(MdbIn, !IO),
	io__output_stream(MdbOut, !IO),
	util__trace_get_command(Prompt, Result, MdbIn, MdbOut, !IO).

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

util__trace_get_command(_, _, _, _, !IO) :-
	private_builtin__sorry("mdb__util__trace_get_command/6").

util__zip_with(Pred, XXs, YYs, Zipped) :-
	( (XXs = [], YYs = []) ->
		Zipped = []
	; (XXs = [X | Xs], YYs = [Y | Ys]) ->
		Pred(X,Y,PXY),
		Zipped = [PXY | Rest],
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
