% This module tests minimal_model evaluation of semidet predicates.
% The question is: "should r(1, Soln1) succeed with Soln1 = 2, or
% should it throw an exception?

:- module semidet_minimal.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

:- import_module std_util, int, exception.

main(!IO) :-
	( r(1, RSoln1) ->
		io__write_string("r(1) = ", !IO),
		io__write_int(RSoln1, !IO),
		io__write_string("\n", !IO)
	;
		io__write_string("r(1) failed\n", !IO)
	),
	( r(2, RSoln2) ->
		io__write_string("r(2) = ", !IO),
		io__write_int(RSoln2, !IO),
		io__write_string("\n", !IO)
	;
		io__write_string("r(2) failed\n", !IO)
	),
	( s(1, SSoln1) ->
		io__write_string("s(1) = ", !IO),
		io__write_int(SSoln1, !IO),
		io__write_string("\n", !IO)
	;
		io__write_string("s(1) failed\n", !IO)
	),
	( s(2, SSoln2) ->
		io__write_string("s(2) = ", !IO),
		io__write_int(SSoln2, !IO),
		io__write_string("\n", !IO)
	;
		io__write_string("s(2) failed\n", !IO)
	),
	( t(1, TSoln1) ->
		io__write_string("t(1) = ", !IO),
		io__write_int(TSoln1, !IO),
		io__write_string("\n", !IO)
	;
		io__write_string("t(1) failed\n", !IO)
	),
	( t(2, TSoln2) ->
		io__write_string("t(2) = ", !IO),
		io__write_int(TSoln2, !IO),
		io__write_string("\n", !IO)
	;
		io__write_string("t(2) failed\n", !IO)
	),
	( v(1, VSoln1) ->
		io__write_string("v(1) = ", !IO),
		io__write_int(VSoln1, !IO),
		io__write_string("\n", !IO)
	;
		io__write_string("v(1) failed\n", !IO)
	),
	( v(2, VSoln2) ->
		io__write_string("v(2) = ", !IO),
		io__write_int(VSoln2, !IO),
		io__write_string("\n", !IO)
	;
		io__write_string("v(2) failed\n", !IO)
	),
	solutions(w(1), WSolns1),
	io__write_string("w(1) = ", !IO),
	io__write(WSolns1, !IO),
	io__nl(!IO),
	solutions(w(2), WSolns2),
	io__write_string("w(2) = ", !IO),
	io__write(WSolns2, !IO),
	io__nl(!IO).

:- pred r(int::in, int::out) is semidet.
:- pragma minimal_model(r/2).

r(A, B) :-
	(
		r(A, _),
		throw("recursive call successful")
	;
		( A = 1 ->
			B = 2
		;
			fail
		)
	).

:- pred s(int::in, int::out) is semidet.
:- pragma minimal_model(s/2).

s(A, B) :-
	( A = 1 ->
		fail
	;
		(
			some [P1, P2] (
				p(P1),
				p(P2),
				P1 \= P2,
				P1 \= A
			)
		->
			r(A, B)
		;
			fail
		)
	).

:- pred t(int::in, int::out) is semidet.
:- pragma minimal_model(t/2).

t(A, B) :-
	u(A, B).

:- pred u(int::in, int::out) is semidet.
:- pragma minimal_model(u/2).

u(A, B) :-
	( A = 0 ->
		B = 0
	; A = 1 ->
		t(A, B)
	;
		t(A - 1, B)
	).

:- pred v(int::in, int::out) is semidet.
:- pragma minimal_model(v/2).

v(A, B) :-
	( some [C] w(A, C) ->
		B = 2
	;
		B = 3
	).

:- pred w(int::in, int::out) is nondet.
:- pragma minimal_model(w/2).

w(A, B) :-
	( some [C] v(A, C) ->
		p(B)
	;
		fail
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The rest of the code is from the coup.m test case.

:- pragma minimal_model(p/1).
:- pred p(int).
:- mode p(out) is nondet.

p(X) :-
	q(X).
p(X) :-
	X = 1.

:- pragma minimal_model(q/1).
:- pred q(int).
:- mode q(out) is nondet.

q(3) :- q(_).
q(4) :- p(_).
