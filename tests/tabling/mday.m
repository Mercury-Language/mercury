:- module mday.
	
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
	( x(_) ->
		write_string("yes\n", !IO)
	;
		write_string("no\n", !IO)
	).

:- pred x(int::out) is nondet.
:- pred y(int::out) is nondet.
:- pred z(int::out) is nondet.
:- pred b(int::out) is multi.
:- pred c(int::out) is multi.
:- pred d(int::out) is det.

:- pragma minimal_model(y/1).
:- pragma minimal_model(z/1).

x(A) :- y(A), d(A).

y(A) :- z(A), z(A).
z(A) :- b(A), c(A).

b(3).
b(4).

c(4).
c(3).

d(3).
