%------------------------------------------------------------------------------
%	Benchmark Program - Counting occurrences in lists
%
%	Author: B. Ramkumar and L. V. Kale
%	Date: 
%
%	To test: run o/1.  
%------------------------------------------------------------------------------
% Benchmark is o(31), runs with -A1 -E256 -C128

:- module uniq_mode_inf_bug.

:- interface.
:- import_module list.

:- pred occurall(list(int), list(list(int)), list(list(int))).
:- mode occurall(in, in, out) is nondet.

:- implementation.

:- import_module int.

occurall([], _X, []).
occurall([X|Y],Z, [[X,W]|V]) :-
	occur(X,Z,W),
	occurall(Y,Z,V).

occur(_X,[],0).
occur(X,[Y|Z],W) :-
	(
		count(X,Y,A),
		occur(X,Z,B)
	->
		W = A + B
	;
		fail
	).

count(_X,[],0).
count(X,[Y|Z],W) :-
	( count(X,Z,W1) ->
		addx(X,Y,W1,W)
	;
		fail
	).

addx(X,X,W1,W) :-
	W = W1 + 1.
addx(X,Y,W1,W1) :-
	X \= Y.
