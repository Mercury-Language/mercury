% This module is used to test the mdb scripts.
%
:- module scripts.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type type1
	--->	type1(int, string).

main(!IO) :-
	p(T),
	io.write(T, !IO),
	io.nl(!IO).

:- pred p(type1::out) is det.

p(T) :- q(T).

:- pred q(type1::out) is det.

q(type1(1, "foo")).
