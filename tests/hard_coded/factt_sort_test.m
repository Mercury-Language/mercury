%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test.
%

:- module factt_sort_test.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list.
:- import_module string.

main(!IO) :-
    test(0x044E, !IO),
    test(0x044F, !IO),
    test(0x0450, !IO),
    test(0x0451, !IO).

:- pred test(int, io, io).
:- mode test(in, di, uo) is det.

test(X, !IO) :-
    ( if unicode_to_big5(X, Y) then
        format("%d => %d\n", [i(X), i(Y)], !IO)
    else
        format("%d => fail\n", [i(X)], !IO)
    ).

:- pred unicode_to_big5(int, int).
:- mode unicode_to_big5(in, out) is semidet.

:- pragma fact_table(unicode_to_big5/2, "factt_sort_test.facts").
