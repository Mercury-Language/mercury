%-----------------------------------------------------------------------------%

% File: std_util.nu.nl.
% Main author: fjh.

%-----------------------------------------------------------------------------%

:- pred report_stats.
report_stats :-
	statistics(L),
	member(global=[Heap,_], L),
	member(local=[Stack,_], L),
	member(trail=[Trail,_], L),
	member(program=[Prog,_], L),
	member(memory=[Total,_], L),
	format("[Heap ~3dk, Stack ~3dk, Trail ~3dk, Prog ~3dk, Tot ~3dk]\n",
		[Heap, Stack, Trail, Prog, Total]),
	fail ; true.

%-----------------------------------------------------------------------------%

:- pred gc_call(pred).
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
:- mode my_member(output, input).
my_member(X, H.T) :-
	my_member_2(T, H, X).

:- pred my_member_2(list(T), T, T).
:- mode my_member_2(input, input, output).

my_member_2([], X, X).
my_member_2(_._, X, X).
my_member_2(H.T, _, X) :-
	my_member_2(T, H, X).

%-----------------------------------------------------------------------------%
