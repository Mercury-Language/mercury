%---------------------------------------------------------------------------%
% Copyright (C) 1994-1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: std_util.nu.nl.
% Main author: fjh.

%-----------------------------------------------------------------------------%

:- pred report_stats.

report_stats :-
	( nuprolog ->
		statistics(L),
		list__member(global=[Heap,_], L),
		list__member(local=[Stack,_], L),
		list__member(trail=[Trail,_], L),
		list__member(program=[Prog,_], L),
		list__member(memory=[Total,_], L),
		list__member(time=[Time,_], L),
		StackPlusTrail is Stack + Trail,
		format(user_error,
			"[Heap ~3dk, Stack+Trail ~3dk, Prog ~3dk, Tot ~3dk, Time ~3d]\n",
			[Heap, StackPlusTrail, Prog, Total, Time])
	;
		statistics(globalused, Heap),
		statistics(localused, Stack),
		statistics(trailused, Trail),
		statistics(cputime, Time),
		format(user_error,
			"[Heap ~3dk, Stack ~3dk, Trail ~3dk, Time ~3f]\n",
			[Heap, Stack, Trail, Time])
	),
	fail ; true.

%-----------------------------------------------------------------------------%

/***
This is no longer used.  It would require polymorphic higher-order
pred modes to be useful.

:- pred gc_call(pred).
:- mode gc_call(pred_call).

gc_call(Goal) :-
	findall(Goal, Goal, List),
	my_member(Goal, List).
***/

%-----------------------------------------------------------------------------%

/* This version of member uses first-argument indexing to avoid creating
   a choice point when a the item matches with the _last_ element in the list.
   So this version will sometimes create fewer choice points than the
   original.
*/
:- pred my_member(T, list(T)).
:- mode my_member(out, in).
my_member(X, [H|T]) :-
	my_member_2(T, H, X).

:- pred my_member_2(list(T), T, T).
:- mode my_member_2(in, in, out).

my_member_2([], X, X).
my_member_2([_|_], X, X).
my_member_2([H|T], _, X) :-
	my_member_2(T, H, X).

%-----------------------------------------------------------------------------%

builtin_solutions(P, L) :-
	findall(X, call(P, X), L).

%-----------------------------------------------------------------------------%

builtin_aggregate(P, A, Acc0, Acc) :-
	findall(X, call(P, X), L),
	list__foldl(A, L, Acc0, Acc).

%-----------------------------------------------------------------------------%

% This is buggy in the backwards mode, but it will just have to do for
% the moment.

type_to_univ(X, X).

%-----------------------------------------------------------------------------%

semidet_succeed :- true.
semidet_fail :- fail.

%-----------------------------------------------------------------------------%
