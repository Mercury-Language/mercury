%-----------------------------------------------------------------------------%

% File: std_util.nu.nl.
% Main author: fjh.

%-----------------------------------------------------------------------------%

:- pred report_stats.

report_stats :-
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
		[Heap, StackPlusTrail, Prog, Total, Time]),
	fail ; true.

%-----------------------------------------------------------------------------%

:- pred gc_call(pred).
:- mode gc_call(pred_call).

gc_call(Goal) :-
	findall(Goal, Goal, List),
	my_member(Goal, List).

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

	% We call findall/3 rather than solutions/3 for efficiency.

solutions(P, L) :-
	findall(X, call(P, X), L).

%-----------------------------------------------------------------------------%

% This is buggy in the backwards mode, but it will just have to do for
% the moment.

type_to_univ(X, X).

%-----------------------------------------------------------------------------%
