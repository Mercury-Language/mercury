%-----------------------------------------------------------------------------%
%
% This is a regression test; mercury 0.6 died with
%
%	$ mc --infer-all mode_inf_bug.m
%	...
%	Software error: modecheck_set_var_inst: unify_inst failed
%
%-----------------------------------------------------------------------------%

:- module mode_inf_bug.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module int, list, bool, char.

main --> test1, test2.

time(Pred, Count, _InternalCount) -->
	( { Pred } ->
		write_s("yes")
	;
		write_s("no")
	),
	io__nl,
	write_s("Count = "), io__write_int(Count), io__nl.

write_s(S) --> io__write_string(S).

:- type hook ---> hook_is_a.

%-----------------------------------------------------------------------------%

%% ===================================================================

%% Description: Speed tests for meta predicates.

%% RCS: $Id: mode_inf_bug.m,v 1.5 2003-05-26 09:00:59 zs Exp $

%%% Simple test predicate --------------------------------------------
:- mode is_a(in) is semidet.
is_a(a).

%%% Non-meta predicate -----------------------------------------------
is_a_list_1([]).
is_a_list_1([F|R]) :-
  is_a(F),
  is_a_list_1(R).

%%% Non-meta predicate with call -------------------------------------
is_a_list_2([]).
is_a_list_2([F|R]) :-
  call(is_a(F)),
  is_a_list_2(R).

%%% Meta predicate with call -----------------------------------------
map_list_1([],_G).
map_list_1([F|R],G) :-
  call(G, F),
  map_list_1(R,G).

%%% Meta predicate with hook -----------------------------------------
map_list_2([],_G).
map_list_2([F|R],G) :-
  map_list_hook(G,F),
  map_list_2(R,G).

map_list_hook(hook_is_a,X) :- is_a(X).

%%% Generate a list of atoms -----------------------------------------
gen_a_list(N,L) :-
	( N = 0 ->
		L = []
	;
		L = [a|R],
		N1 = N - 1,
		gen_a_list(N1,R)
	).

%%% Test suite -------------------------------------------------------

%% For an alternative, see the definition of cpu_time/3 in "The Craft
%% of Prolog", by Richard A. O'Keefe.
% :- use_module(library(benchmark),[time/3]).

:- mode test_is_a_list_1(in) is semidet.
:- mode test_is_a_list_2(in) is semidet.
:- mode test_map_list_1(in) is semidet.
:- mode test_map_list_2(in) is semidet.
test_is_a_list_1(N) :- gen_a_list(N,L), is_a_list_1(L).
test_is_a_list_2(N) :- gen_a_list(N,L), is_a_list_2(L).
test_map_list_1(N)  :- gen_a_list(N,L), map_list_1(L,is_a).
test_map_list_2(N)  :- gen_a_list(N,L), map_list_2(L,hook_is_a).

test_all(ListSize,Count,InternalCount) -->
  write_s("Non-meta predicate:"),
  time(test_is_a_list_1(ListSize),Count,InternalCount),io__nl,
  %%
  write_s("Non-meta predicate with call:"),
  time(test_is_a_list_2(ListSize),Count,InternalCount),io__nl,
  %%
  write_s("Meta-predicate with call:"),
  time(test_map_list_1(ListSize),Count,InternalCount),io__nl,
  %%
  write_s("Meta-predicate with hook:"),
  time(test_map_list_2(ListSize),Count,InternalCount).

test1 --> test_all(1000,10,10).
test2 --> test_all(10000,10,10).

%-----------------------------------------------------------------------------%
