%
% This module, originally written by Philip Dart,
% uncovered a bug in the implementation of semidet predicates
% in Mercury version 0.4.
%

:- module semidet_map.
:- interface.
:- import_module std_util, list, io.

:- pred main(io__state::di, io__state::uo) is det.

:- pred meta_semidet_map(pred(X, Y), list(X), list(maybe(Y))).
:- mode meta_semidet_map(pred(in, out) is semidet, in, out) is det.

:- implementation.
:- import_module builtin, int, string.

main -->
	( {meta_semidet_map(pos_inc, [0, 1], [X, Y])} ->
		{maybe_to_string(string__int_to_string, X, SX)},
		{maybe_to_string(string__int_to_string, Y, SY)},
		io__write_strings(["Success: X = ", SX, "; Y = ", SY, ".\n"])
	;	io__write_string("Failure.\n")
	).

:- pred maybe_to_string(pred(X, string), maybe(X), string).
:- mode maybe_to_string(pred(in, out) is det, in, out) is det.
maybe_to_string(_, no, "no").
maybe_to_string(P, yes(T), S) :-
	call(P, T, S0), string__append_list(["yes(", S0, ")"], S).

:- pred pos_inc(int::in, int::out) is semidet.
pos_inc(X, Y) :- X > 0, Y is X + 1.

meta_semidet_map(_, [],  []).
meta_semidet_map(P, [H0|T0], [H|T]) :-
	( call(P, H0, H1) -> H = yes(H1) ; H = no ),
	meta_semidet_map(P, T0, T).

