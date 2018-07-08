%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A test case to exercise the code for expanding hash tables,
% and for tabling bitmaps.

:- module expand_bitmap.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bitmap.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module random.
:- import_module require.
:- import_module std_util.

:- pragma require_feature_set([memo]).

:- type record(T1, T2)
    --->    record(T1, T1, T2).

main(!IO) :-
    random.init(0, RS0),
    random.permutation(range(0, 1023), Perm, RS0, RS1),

    BM1 = bitmap.init(45, yes) ^ bits(20, 8) := 0b10001001,
    BM2 = bitmap.init(123, no) ^ bits(10, 8) := 0b10101010,
    choose_signs_and_enter(Perm, BM1, Solns1, RS1, RS2),
    ( if test_tables(Solns1, yes) then
        io.write_string("First test successful.\n", !IO)
    else
        io.write_string("First test unsuccessful.\n", !IO)
    ),
    choose_signs_and_enter(Perm, BM2, Solns2, RS2, _RS),
    ( if test_tables(Solns2, yes) then
        io.write_string("Second test successful.\n", !IO)
    else
        io.write_string("Second test unsuccessful.\n", !IO)
    ).
    % io.report_tabling_stats(!IO).

:- func range(int, int) = list(int).

range(Min, Max) =
    ( if Min > Max then
        []
    else
        [Min | range(Min + 1, Max)]
    ).

:- pred choose_signs_and_enter(list(int)::in, T::in, list(record(int, T))::out,
    random.supply::mdi, random.supply::muo) is det.

choose_signs_and_enter([], _, [], RS, RS).
choose_signs_and_enter([N | Ns], A, [record(F, S, A) | ISs], RS0, RS) :-
    random.random(Random, RS0, RS1),
    ( if Random mod 2 = 0 then
        F = N
    else
        F = 0 - N
    ),
    sum(F, A, S),
    choose_signs_and_enter(Ns, A, ISs, RS1, RS).

:- pred test_tables(list(record(int, T))::in, bool::out) is det.

test_tables([], yes).
test_tables([record(I, S0, A) | Is], Correct) :-
    sum(I, A, S1),
    ( if S0 = S1 then
        test_tables(Is, Correct)
    else
        Correct = no
    ).

:- pred sum(int::in, T::in, int::out) is det.
:- pragma memo(sum/3).

sum(N, A, F) :-
    ( if N < 0 then
        sum(0 - N, A, NF),
        F = 0 - NF
    else if N = 1 then
        F = 1
    else
        sum(N - 1, A, F1),
        F = N + F1
    ).
