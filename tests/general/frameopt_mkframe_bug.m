/*
$ mc bug.m
Software error: mkframe in frameopt__doit
*/

:- module frameopt_mkframe_bug.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.
:- import_module int, bool.

main -->
	{ p1(X1) }, io__write_int(X1), io__write_string("\n"),
	{ p2(X2) }, io__write_int(X2), io__write_string("\n").

:- pred p1(int::out) is multi.
p1(X) :-
	q(FindMe),
	( u(41) ->
		Z = 1
	;
		( r(Z) ; s(FindMe, Z) )
	),
	t(FindMe, Z, X).

:- pred p2(int::out) is multi.
p2(X) :-
	q(Y2),
	(
%		( u(41) ->
%			Z = 1
%		;
			Z = 111
%		)
	;
		( r(Z) ; s(Y2, Z) )
	),
	t(Y2, Z, X).

:- pred q(int::out) is det.
:- pred r(int::out) is det.
:- pred s(int::in, int::out) is det.
:- pred t(int::in, int::in, int::out) is det.
:- pred u(int::in) is semidet.
:- pred v(bool::out) is det.

q(2).
r(3).
s(X, Y4) :- Y4 is X + 10.
t(X, Y5, Z) :- Z is X * 100 + Y5.
u(A) :-
	A > 30.
v(yes).
