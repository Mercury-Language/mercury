	%
	% Tests that disjunctions gets handled properly.
	%
:- module disj.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

:- import_module int, list, solutions.

:- type wrapper ---> w(int, int, int).

main -->
	{ solutions(p([1,7,4]), SumList) },
	io__write_string("p: "),
	io__write(SumList),
	io__nl,
	{ pa([1,7,4], FirstSoln) },
	io__write_string("First soln p: "),
	io__write(FirstSoln),
	io__nl,
	{ solutions(p2([1,7,4]), SumList2) },
	io__write_string("p2: "),
	io__write(SumList2),
	io__nl,
	{ solutions(p3([1,7,4]), SumList3) },
	io__write_string("p3: "),
	io__write(SumList3),
	io__nl,
	{ solutions(p4a([1,7,4]), SumList4) },
	io__write_string("p4: "),
	io__write(SumList4),
	io__nl.

:- pred pa(list(int), int).
:- mode pa(in, out) is cc_multi.

pa(X, Y) :-
	p(X,Y).

:- pred p(list(int), int).
:- mode p(in, out) is multi.

	%
	% Introduce accumulators because each arm of the disjunction
	% will always produce the same value.
	%
p([], 0).
p([H|T], Sum) :-
	p(T, Sum0),
	(
		Tmp = 2*H
	;
		Tmp = 3*H
	),
	Sum is Sum0 + Tmp.

	%
	% In the second arm of the disjunction, the call 
	% (Sum is Sum0 + Tmp) contains 2 dynamic vars so we should fail.
	%
:- pred p2(list(int), int).
:- mode p2(in, out) is nondet.

p2([], 0).
p2([H|T], Sum) :-
	p2(T, Sum0),
	(
		Tmp = 2*H
	;
		Tmp = H*Sum0
	),
	Sum is Sum0 + Tmp.


:- pred p3(list(int), int).
:- mode p3(in, out) is nondet.

p3([], 0).
p3([H|T], Sum) :-
	p3(T, Sum0),
	(
		Tmp = 0
	;
		Tmp = Sum0
	),
	Sum is H + Tmp.

:- pred p4a(list(int), wrapper).
:- mode p4a(in, out) is nondet.

p4a(X, Y) :-
	p4(X,S,L,NDS),
	Y = w(S,L,NDS).

:- pred p4(list(int), int, int, int).
:- mode p4(in, out, out, out) is nondet.

p4([], 0, 0, 0).
p4([H|T], Sum, Length, NonDetSum) :-
	p4(T, Sum0, Length0, NonDetSum0),
	Length is Length0 + 1,
	Sum is H + Sum0,
	(
		Tmp = Length0
	;
		Tmp = Sum0
	;
		Tmp = NonDetSum0
	),
	NonDetSum is H + Tmp.
