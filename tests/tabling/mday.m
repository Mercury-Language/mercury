:- module mday.
	
:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main(IO0, IO) :-
	( a(_) ->
		write_string("yes\n", IO0, IO)
	;
		write_string("no\n", IO0, IO)
	).

:- pred a(int::out) is nondet.
:- pred b(int::out) is multi.
:- pred c(int::out) is multi.
:- pred d(int::out) is det.
:- pred a1(int::out) is nondet.
:- pred a2(int::out) is nondet.

:- pragma minimal_model(a1/1).
:- pragma minimal_model(a2/1).

a(A) :- a2(A), d(A).

a2(A) :- a1(A), a1(A).
a1(A) :- b(A), c(A).

b(3).
b(4).

c(4).
c(3).

d(3).
