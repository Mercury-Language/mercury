%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test whether functions and predicates in one_or_more.m behave the same way
% as their equivalents in list.m.
%
%---------------------------------------------------------------------------%

:- module test_one_or_more.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module one_or_more.
:- import_module random.
:- import_module random.sfc16.
:- import_module solutions.
:- import_module string.

main(!IO) :-
    some [!RandomState] (
        !:RandomState = random.sfc16.init,
        generate_random_oom(20, 100, OoMA, !RandomState),
        generate_random_oom(30, 100, OoMB, !RandomState),
        generate_random_oom(25, 100, OoMC, !RandomState),
        generate_random_oom(100, 20, DenseOoMA, !RandomState),
        generate_random_oom(100, 20, DenseOoMB, !RandomState),
        generate_random_oom(100, 20, DenseOoMC, !RandomState),
        _ = !.RandomState
    ),

    test_member("A", OoMA, !IO),
    test_member("B", OoMB, !IO),
    test_member("C", OoMC, !IO),

    test_simple_func("sort_and_remove_dups", "A", DenseOoMA,
        one_or_more.sort_and_remove_dups, list.sort_and_remove_dups, !IO),
    test_simple_func("sort_and_remove_dups", "B", DenseOoMB,
        one_or_more.sort_and_remove_dups, list.sort_and_remove_dups, !IO),
    test_simple_func("sort_and_remove_dups", "C", DenseOoMC,
        one_or_more.sort_and_remove_dups, list.sort_and_remove_dups, !IO).

:- pred test_member(string::in, one_or_more(int)::in,
    io::di, io::uo) is det.

test_member(Id, OoM, !IO) :-
    List = one_or_more_to_list(OoM),
    PredOoM =
        ( pred(X::out) is multi :-
            one_or_more.member(X, OoM)
        ),
    PredList =
        ( pred(X::out) is nondet :-
            list.member(X, List)
        ),
    solutions.solutions(PredOoM, SolnsOoM),
    solutions.solutions(PredList, SolnsList),
    ( if SolnsOoM = SolnsList then
        io.format("member test on %s succeeded\n", [s(Id)], !IO)
    else
        io.format("member test on %s failed\n", [s(Id)], !IO)
    ).

:- pred test_simple_func(string::in, string::in, one_or_more(int)::in,
    (func(one_or_more(int)) = one_or_more(T))::in(func(in) = out is det),
    (func(list(int)) = list(T))::in(func(in) = out is det),
    io::di, io::uo) is det.

test_simple_func(Test, Id, OoM, FuncOoM, FuncList, !IO) :-
    List = one_or_more_to_list(OoM),
    ( if one_or_more_to_list(FuncOoM(OoM)) = FuncList(List) then
        io.format("%s test on %s succeeded\n", [s(Test), s(Id)], !IO)
    else
        io.format("%s test on %s failed\n", [s(Test), s(Id)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred generate_random_oom(int::in, int::in, one_or_more(int)::out,
    RandomState::in, RandomState::out) is det <= random(RandomState).

generate_random_oom(Count, Max, OoM, !RandomState) :-
    uniform_int_in_range(1, Max, H, !RandomState),
    generate_random_list(Count - 1, Max, [], T, !RandomState),
    OoM = one_or_more(H, T).

:- pred generate_random_list(int::in, int::in,
    list(int)::in, list(int)::out, RandomState::in, RandomState::out) is det
    <= random(RandomState).

generate_random_list(Count, Max, List0, List, !RandomState) :-
    ( if Count > 0 then
        uniform_int_in_range(1, Max, R, !RandomState),
        generate_random_list(Count - 1, Max, [R | List0], List, !RandomState)
    else
        List = List0
    ).

%---------------------------------------------------------------------------%
