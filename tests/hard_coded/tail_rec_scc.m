%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module tail_rec_scc.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    p(42, 0, X),
    io.format("%d\n", [i(X)], !IO).

:- pred p(int::in, int::in, int::out) is det.
:- pred q(int::in, int::in, int::out) is det.
:- pred r(int::in, int::in, int::out) is det.

p(A, B, C) :-
    ( if A = 0 then
        C = B
    else
        q(A - 1, B + 1, C)
    ).

q(A, B, C) :-
    ( if A = 0 then
        C = B
    else
        r(A - 1, B + 1, C)
    ).

r(A, B, C) :-
    ( if A = 0 then
        C = B
    else
        p(A - 1, B + 1, C)
    ).
