% File: higher_order.m.
% Author: fjh.
%
% Some very basic tests of higher-order predicates and lambda expressions.

:- module higher_order.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module string.

:- pred map(pred(T1, T2), list(T1), list(T2)).
:- mode map(pred(in, out) is det, in, out) is det.
:- mode map(pred(in, in) is semidet, in, in) is semidet.

map(_Pred, [], []).
map(Pred, [X|Xs], [Y|Ys]) :-
	call(Pred, X, Y),
	map(Pred, Xs, Ys).

:- pred double(string::in, string::out) is det.
double(X, Y) :-
	string__append(X, X, Y).

main --> 
	{ map(double, ["foo", "bar"], List) },
	io__write_strings(List),
	io__write_string("\n"),
	(
		{ map(lambda([X::in, Y::in] is semidet, double(X, Y)),
			["ab"], ["abab"]) }
	->
		io__write_string("Yes\n")
	;
		io__write_string("Oops\n")
	),
	(
		{ map(lambda([X::in, Y::in] is semidet, double(X, Y)),
			["ab"], ["abracadabra"]) }
	->
		io__write_string("Oops\n")
	;
		io__write_string("No\n")
	),
	(
		{ map(lambda([X::in, Y::in] is semidet, double(X, Y)),
			["ab"], []) }
	->
		io__write_string("Oops\n")
	;
		io__write_string("No\n")
	).

