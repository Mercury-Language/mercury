% Test curried functions.
% This is a regression test: mercury-0.6 failed this test.

:- module curry.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module list, int.

main -->
 	io__write_string("Hello, world\n"),
	{ _ = map(curry2(append), [[1],[2],[3]]) },
	{ _ = map(curry2(plus), [1,2,3]) }.

:- func append(list(T), list(T)) = list(T).
append(A, B) = C :- list__append(A, B, C).

:- inst func1 = (func(in) = out is det).

:- func curry2(func(T1, T2) = T3) = (func(T1) = (func(T2) = T3)).
:- mode curry2(func(in, in) = out is det) =
			out(func(in) = out(func(in) = out is det) is det).
curry2(F) = ((func(X::in) = (F1::out((func(in) = out is det))) is det) :-
		F1 = (func(Y) = apply(F, X, Y))).

:- func plus(int, int) = int.
plus(A, B) = A + B.

:- func map(func(T1) = T2, list(T1)) = list(T2).
% :- mode map(func(in) = out is det, in) = out is det.
:- mode map(func(in) = out(func(in) = out is det) is det, in) = out is det.

map(_F, []) = [].
map(F, [X|Xs]) = [apply(F,X)|map(F, Xs)].

