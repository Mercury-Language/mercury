%
% This module, originally written by Philip Dart,
% uncovered a (second) bug in the implementation of semidet predicates
% in Mercury version 0.4.
%

:- module parse_list.
:- interface.
:- import_module std_util, list, io.

:- pred main(io__state::di, io__state::uo) is det.

:- pred meta_parse_list(pred(Y, Y, X), list(X), Y, Y).
:- mode meta_parse_list(pred(in, out, out) is semidet, out, in, out) is det.

:- implementation.
:- import_module builtin, int, string.

main -->
	{P = lambda([I::in, O::out, N::out] is semidet, one_or_two(N, I, O))},
	( {meta_parse_list(P, [X, Y], [2, 1, 3], _)} ->
		{string__int_to_string(X, SX)},
		{string__int_to_string(Y, SY)},
		io__write_strings(["Success: X = ", SX, "; Y = ", SY, ".\n"])
	;	io__write_string("Failure.\n")
	).

meta_parse_list(P, L, In, Out) :-
	( call(P, In, Out0, E) ->
		L = [E|L1], meta_parse_list(P, L1, Out0, Out)
	;	L = [], Out = In
	).

:- pred one_or_two(int::out, list(int)::in, list(int)::out) is semidet.
one_or_two(1) --> [1].
one_or_two(2) --> [2].


