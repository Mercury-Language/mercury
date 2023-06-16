%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module factt_non.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module solutions.
:- import_module string.

:- pred example(int, float, string).
:- mode example(in, in, in) is semidet.
:- mode example(in, in, out) is nondet.
:- mode example(in, out, out) is nondet.
:- mode example(out, out, out) is multi.

:- pragma fact_table(example/3, "factt_non_examples").

main(!IO) :-
    test_in_in_in(Result1),
    test_in_in_out(Result2),
    test_in_out_out(Result3),
    test_out_out_out(Result4),
    io.print_line(Result1, !IO),
    io.print_line(Result2, !IO),
    io.print_line(Result3, !IO),
    io.print_line(Result4, !IO).

:- pred test_in_in_in(pair(bool)::out) is det.

test_in_in_in(Res1 - Res2) :-
    Res1 = ( if example(2, 2.0, "2.0") then yes else no ),
    Res2 = ( if example(42, 2.0, "foobar") then yes else no ).

:- pred test_in_in_out(pair(maybe(string))::out) is cc_multi.

test_in_in_out(Res1 - Res2) :-
    Res1 = ( if example(42, 3.0, S1) then yes(S1) else no ),
    Res2 = ( if example(42, 2.0, S2) then yes(S2) else no ).

:- pred test_in_out_out(pair(maybe(string))::out) is cc_multi.

test_in_out_out(Res1 - Res2) :-
    ( if
        example(42, F1, S1),
        F1 > 10.0
    then
        Res1 = yes(S1)
    else
        Res1 = no
    ),
    ( if
        example(2, F2, S2),
        F2 > 10.0
    then
        Res2 = yes(S2)
    else
        Res2 = no
    ).

:- pred test_out_out_out(list(string)::out) is det.

test_out_out_out(Res) :-
    Pred =
        ( pred(S::out) is nondet :-
            example(N, F, S),
            ( N > 10
            ; F < 1.5
            )
        ),
    solutions(Pred, Res).
