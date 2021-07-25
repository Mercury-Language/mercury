%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test; mercury 0.6 died with
%
%   $ mc --infer-all mode_inf_bug.m
%   ...
%   Software error: modecheck_set_var_inst: unify_inst failed
%
%---------------------------------------------------------------------------%

:- module mode_inf_bug.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.

main(!IO) :-
    test1(!IO),
    test2(!IO).

%%% Test suite -------------------------------------------------------

%% For an alternative, see the definition of cpu_time/3 in "The Craft
%% of Prolog", by Richard A. O'Keefe.
% :- use_module(library(benchmark), [time/3]).

test1(!IO) :-
    test_all(1000, 10, 10, !IO).

test2(!IO) :-
    test_all(10000, 10, 10, !IO).

test_all(ListSize, Count, InternalCount, !IO) :-
  write_s("Non-meta predicate:", !IO),
  time(test_is_a_list_1(ListSize), Count, InternalCount, !IO),
  io.nl(!IO),

  write_s("Non-meta predicate with call:", !IO),
  time(test_is_a_list_2(ListSize), Count, InternalCount, !IO),
  io.nl(!IO),

  write_s("Meta-predicate with call:", !IO),
  time(test_map_list_1(ListSize), Count, InternalCount, !IO),
  io.nl(!IO),

  write_s("Meta-predicate with hook:", !IO),
  time(test_map_list_2(ListSize), Count, InternalCount, !IO).

:- mode test_is_a_list_1(in) is semidet.
test_is_a_list_1(N) :-
    gen_a_list(N, L),
    is_a_list_1(L).

:- mode test_is_a_list_2(in) is semidet.
test_is_a_list_2(N) :-
    gen_a_list(N, L),
    is_a_list_2(L).

:- mode test_map_list_1(in) is semidet.
test_map_list_1(N) :-
    gen_a_list(N, L),
    map_list_1(L, is_a).

:- mode test_map_list_2(in) is semidet.
test_map_list_2(N) :-
    gen_a_list(N, L),
    map_list_2(L, hook_is_a).

time(Pred, Count, _InternalCount, !IO) :-
    ( if Pred then
        write_s("yes", !IO)
    else
        write_s("no", !IO)
    ),
    io.nl(!IO),
    write_s("Count = ", !IO),
    io.write_int(Count, !IO),
    io.nl(!IO).

write_s(S, !IO) :-
    io.write_string(S, !IO).

:- type hook
    --->    hook_is_a.

%---------------------------------------------------------------------------%

%% Description: Speed tests for meta predicates.

%%% Simple test predicate --------------------------------------------

:- mode is_a(in) is semidet.
is_a(a).

%%% Non-meta predicate -----------------------------------------------

is_a_list_1([]).
is_a_list_1([F | R]) :-
  is_a(F),
  is_a_list_1(R).

%%% Non-meta predicate with call -------------------------------------

is_a_list_2([]).
is_a_list_2([F | R]) :-
  call(is_a(F)),
  is_a_list_2(R).

%%% Meta predicate with call -----------------------------------------

map_list_1([], _G).
map_list_1([F | R], G) :-
  call(G, F),
  map_list_1(R, G).

%%% Meta predicate with hook -----------------------------------------

map_list_2([], _G).
map_list_2([F | R], G) :-
  map_list_hook(G, F),
  map_list_2(R, G).

map_list_hook(hook_is_a, X) :-
    is_a(X).

%%% Generate a list of atoms -----------------------------------------

gen_a_list(N, L) :-
    gen_a_list(N, [], L).

gen_a_list(N, L0, L) :-
    ( if N = 0 then
        L = L0
    else
        N1 = N - 1,
        gen_a_list(N1, [a | L0], L)
    ).

%---------------------------------------------------------------------------%
