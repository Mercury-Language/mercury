% File: higher_order.m.
% Author: fjh.
%
% Some very basic tests of higher-order predicates and lambda expressions.

:- module higher_order.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module string, list.

:- pred map(pred(T1, T2), list(T1), list(T2)).
:- mode map(pred(in, out) is det, in, out) is det.
:- mode map(pred(in, in) is semidet, in, in) is semidet.

higher_order__map(_Pred, [], []).
higher_order__map(Pred, [X|Xs], [Y|Ys]) :-
	call(Pred, X, Y),
	higher_order__map(Pred, Xs, Ys).

:- pred double(string::in, string::out) is det.
double(X, Y) :-
	string__append(X, X, Y).

main --> 
	{ higher_order__map(double, ["foo", "bar"], List) },
	io__write_strings(List),
	io__write_string("\n"),
	(
		{ higher_order__map(lambda([X::in, Y::in] is semidet,
			double(X, Y)), ["ab"], ["abab"]) }
	->
		io__write_string("Yes\n")
	;
		io__write_string("Oops\n")
	),
	(
		{ higher_order__map(lambda([X::in, Y::in] is semidet,
			double(X, Y)), ["ab"], ["abracadabra"]) }
	->
		io__write_string("Oops\n")
	;
		io__write_string("No\n")
	),
	(
		{ higher_order__map(lambda([X::in, Y::in] is semidet,
			double(X, Y)), ["ab"], []) }
	->
		io__write_string("Oops\n")
	;
		io__write_string("No\n")
	).

