:- module filter.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module list, int.

main -->
	(
		{ p(X), q(X) }
	->
		io__write_string("yes\n")
	;
		io__write_string("no\n")
	).

:- pred p(list(int)).
:- mode p(out) is multi.

p(X) :-
	s1(A),
	s2(B),
	my_append(A, B, X).

:- pred s1(list(int)).
:- mode s1(out) is multi.

s1([1, 2]).
s1([1, 2, 3]).

:- pred s2(list(int)).
:- mode s2(out) is multi.

s2([9]).
s2([7, 8, 9]).

:- pred q(list(T)).
:- mode q(in) is semidet.

q(X) :-
	my_length(X, L),
	L > 6.

:- pred my_append(list(T), list(T), list(T)).
:- mode my_append(in, in, out) is det.

my_append(A, B, C) :-
	list__append(A, B, C).

:- pred my_length(list(T), int).
:- mode my_length(in, out) is det.

my_length(A, L) :-
	list__length(A, L).

