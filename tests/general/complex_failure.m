% This test case exercises the code generator's handling of if-then-elses
% whose conditions include nondet disjunctions, looking for problems caused
% by the disjunction's clobbering some of the same redoip/redofr slots used
% for the soft cut in the if-then-else.

:- module complex_failure.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module std_util, list, int.

main -->
	{ solutions(p1, Xs1) },
	print_list(Xs1),
	{ solutions(p2, Xs2) },
	print_list(Xs2),
	{ solutions(p3, Xs3) },
	print_list(Xs3).

:- pred p1(int::out) is nondet.

p1(X) :-
	p(1, X).

:- pred p2(int::out) is nondet.

p2(X) :-
	p(2, X).

:- pred p3(int::out) is nondet.

p3(X) :-
	p(3, X).

:- pred p(int::in, int::out) is nondet.

p(A, X) :-
	% The first if-then-else can hijack the redoip/redofr slots
	% of p's frame.
	( if
		some [B] ( q(A, B) ; r(A, B) )
	then
		C is B * 10
		% s(B, C)
	else
		C is A * 10
		% s(A, C)
	),
	% The second if-then-else cannot hijack the redoip/redofr slots
	% of p's frame, since this may not be (and usually won't be)
	% on top when execution gets here.
	( if
		some [D] ( q(C, D) ; r(C, D) )
	then
		s(D, X)
	else
		s(C, X)
	).

:- pred q(int::in, int::out) is nondet.

q(1, 15).
q(1, 16).
q(2, 25).
q(2, 26).
q(3, 35).

q(150, 660).
q(161, 661).
q(281, 662).

:- pred r(int::in, int::out) is nondet.

r(1, 18).
r(1, 19).
r(2, 28).
r(2, 29).
r(3, 38).
r(260, 690).
r(370, 698).

:- pred s(int::in, int::out) is nondet.

s(F, G) :-
	F < 695,
	(
		G is 10 * F
	;
		G is 10 * F + 1
	).

:- pred print_list(list(int), io__state, io__state).
:- mode print_list(in, di, uo) is det.

print_list(Xs) -->
	(
		{ Xs = [] }
	->
		io__write_string("[]\n")
	;
		io__write_string("["),
		print_list_2(Xs),
		io__write_string("]\n")
	).

:- pred print_list_2(list(int), io__state, io__state).
:- mode print_list_2(in, di, uo) is det.

print_list_2([]) --> [].
print_list_2([X|Xs]) --> 
	io__write_int(X),
	(
		{ Xs = [] }
	->
		[]
	;
		io__write_string(", "),
		print_list_2(Xs)
	).
