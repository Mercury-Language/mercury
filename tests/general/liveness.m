% This is a regression test for a bug in liveness.m.
% 
% When stuffing liveness residues after goals, which we do for
% variables which are nondet-live in one arm of a branched goal
% but not in the other, make sure that we do not add post-births
% for variables that are already live at the end of the goal that
% we're doing the stuffing into.  (If we do that, then the
% compiler assumes that they have become automagically live, and
% so just assumes they are in some random register.)

:- module liveness.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.
:- import_module int, bool.

main -->
	{ p1(X1) }, io__write_int(X1), io__write_string("\n"),
	{ p2(X2) }, io__write_int(X2), io__write_string("\n"),
	{ p3(X3) }, io__write_int(X3), io__write_string("\n").

:- pred p1(int::out) is multi.
p1(X) :-
	q(FindMe),
	( u(41,42,43,44,45) ->
		Z = 1
	;
		( r(Z) ; s(FindMe, Z) )
	),
	t(FindMe, Z, X).

:- pred p2(int::out) is multi.
p2(X) :-
	q(Y2),
	(
		( u(41,42,43,44,45) ->
			Z = 1
		;
			Z = 111
		)
	;
		( r(Z) ; s(Y2, Z) )
	),
	t(Y2, Z, X).

:- pred p3(int::out) is multi.
p3(X) :-
	q(Y3),
	v(Bool),
	(	Bool = yes,
		( u(41,42,43,44,45) ->
			Z = 1
		;
			Z = 111
		)
	;
		Bool = no,
		( r(Z) ; s(Y3, Z) )
	),
	t(Y3, Z, X).

:- pred q(int::out) is det.
:- pred r(int::out) is det.
:- pred s(int::in, int::out) is det.
:- pred t(int::in, int::in, int::out) is det.
:- pred u(int::in, int::in, int::in, int::in, int::in) is semidet.
:- pred v(bool::out) is det.

q(2).
r(3).
s(X, Y4) :- Y4 is X + 10.
t(X, Y5, Z) :- Z is X * 100 + Y5.
u(A,B,C,D,E) :-
	Sum is A+B+C+D+E,
	Sum > 200.
v(yes).
