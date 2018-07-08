% vim: ts=4 sw=4 et ft=mercury
%
% A test case to exercise the code for handling types that are statically known
% to be tuples.
%
% The test is a modified version of expand_tuple.

:- module expand_tuple2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module random.
:- import_module require.
:- import_module std_util.
:- import_module string.

:- pragma require_feature_set([memo]).

:- type record(T1, T2)
    --->  record(T1, T2, T1, T2).

main(!IO) :-
    random.init(0, RS0),
    random.permutation(range(0, 1023), Perm, RS0, RS1),
    choose_signs_and_enter(Perm, "0", Solns1, RS1, _RS2),
    ( if test_tables(Solns1, yes) then
        io.write_string("Test successful.\n", !IO)
    else
        io.write_string("Test unsuccessful.\n", !IO)
    ).
    % io.report_tabling_stats(!IO).

:- func range(int, int) = list(int).

range(Min, Max) =
    ( if Min > Max then
        []
    else
        [Min | range(Min + 1, Max)]
    ).

:- pred choose_signs_and_enter(list(int)::in, string::in,
    list(record(int, string))::out,
    random.supply::mdi, random.supply::muo) is det.

choose_signs_and_enter([], _, [], RS, RS).
choose_signs_and_enter([N | Ns], A, [record(F, A, S, B) | ISs], RS0, RS) :-
    random.random(Random, RS0, RS1),
    ( if Random mod 2 = 0 then
        F = N
    else
        F = 0 - N
    ),
    sum({F, A}, {S, B}),
    choose_signs_and_enter(Ns, A, ISs, RS1, RS).

:- pred test_tables(list(record(int, string))::in, bool::out) is det.

test_tables([], yes).
test_tables([record(F, A, S0, B0) | Is], Correct) :-
    sum({F, A}, {S1, B1}),
    ( if S0 = S1, B0 = B1 then
        test_tables(Is, Correct)
    else
        Correct = no
    ).

:- pred sum({int, string}::in, {int, string}::out) is det.
:- pragma memo(sum/2).

sum({N, A}, {S, B}) :-
    ( if N < 0 then
        sum({0 - N, A}, {S0, B0}),
        S = 0 - S0,
        B = "-" ++ B0
    else if N = 0 then
        S = 0,
        B = A
    else
        sum({N - 1, A}, {S0, B0}),
        S = S0 + 1,
        B = B0 ++ "+"
    ).
