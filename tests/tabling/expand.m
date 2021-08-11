%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A test case to exercise the code for expanding hash tables.

:- module expand.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module random.
:- import_module random.sfc32.
:- import_module uint32.

:- pragma require_feature_set([memo]).

main(!IO) :-
    sfc32.init(RNG, RS0),
    random.shuffle_list(RNG, range(0, 1023), Perm, RS0, RS1),
    choose_signs_and_enter(RNG, Perm, Solns, RS1, _RS),
    ( if test_tables(Solns, yes) then
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

:- pred choose_signs_and_enter(RNG::in, list(int)::in, assoc_list(int)::out,
    State::di, State::uo) is det <= urandom(RNG, State).

choose_signs_and_enter(_, [], [], !RS).
choose_signs_and_enter(RNG, [N | Ns], [I - S | ISs], !RS) :-
    random.generate_uint32(RNG, Random, !RS),
    ( if Random mod 2u32 = 0u32 then
        I = N
    else
        I = 0 - N
    ),
    sum(I, S),
    choose_signs_and_enter(RNG, Ns, ISs, !RS).

:- pred test_tables(assoc_list(int)::in, bool::out) is det.

test_tables([], yes).
test_tables([I - S0 | Is], Correct) :-
    sum(I, S1),
    ( if S0 = S1 then
        test_tables(Is, Correct)
    else
        Correct = no
    ).

:- pred sum(int::in, int::out) is det.
:- pragma memo(sum/2).

sum(N, F) :-
    ( if N < 0 then
        sum(0 - N, NF),
        F = 0 - NF
    else if N = 1 then
        F = 1
    else
        sum(N - 1, F1),
        F = N + F1
    ).
