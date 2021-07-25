%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ho4.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.

main(!IO) :-
    ( if p(1) then
        S1 = ""
    else
        S1 = "\\+ "
    ),
    ( if p(2) then
        S2 = ""
    else
        S2 = "\\+ "
    ),
    io.write_strings([S1, "p(1).\n", S2, "p(2).\n"], !IO).

:- pred p(int::in) is semidet.

p(N) :-
    q(r(5), M),
    M = N.

:- pred q(pred(int)::in(pred(out) is multi), int::out) is multi.

q(R, N) :-
    R(N).

:- pred r(int::in, int::out) is multi.

r(A, A).
r(_, 0).
