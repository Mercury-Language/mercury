%---------------------------------------------------------------------------%
% Copyright (C) 1998-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module util.

:- interface.

:- import_module list, string, char, io.

	% Get user input via the same method used by the internal
	% debugger.
:- pred util__trace_getline(string, io__result(list(char)), io__state,
		io__state).
:- mode util__trace_getline(in, out, di, uo) is det.

:- pred util__zip_with(pred(T1, T2, T3), list(T1), list(T2), list(T3)).
:- mode util__zip_with(pred(in, in, out) is det, in, in, out) is det.

	% Apply predicate to argument repeatedly until the result
	% remains the same.
:- pred util__limit(pred(list(T), list(T)), list(T), list(T)).
:- mode util__limit(pred(in,out) is det, in, out) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module int, require.

:- pragma promise_pure(util__trace_getline/4).

util__trace_getline(Prompt, Result) -->
	{
		impure call_trace_getline(Prompt, Line)
	->
		string__to_char_list(Line, Chars),
		Result = ok(Chars)
	;
		Result = eof
	}.

:- impure pred call_trace_getline(string, string).
:-        mode call_trace_getline(in, out) is semidet.

:- pragma c_header_code("
	#include ""mercury_wrapper.h""
	#include ""mercury_string.h""
	#include ""mercury_trace_base.h""
	#include ""mercury_trace_internal.h""
").

:- pragma c_code(call_trace_getline(Prompt::in, Line::out),
	[will_not_call_mercury],
	"
		char		*line;
		char		*mercury_string;

		if (MR_address_of_trace_getline != NULL) {
			line = (*MR_address_of_trace_getline)((char *) Prompt);
		} else {
			MR_tracing_not_enabled();
			/* not reached */
		}

		if (line == NULL) {
			SUCCESS_INDICATOR = FALSE;
		} else {
			make_aligned_string_copy(mercury_string, line);
			free(line);
			Line = (String) mercury_string;
			SUCCESS_INDICATOR = TRUE;
		}
	"
).

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
