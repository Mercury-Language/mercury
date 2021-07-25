%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ho3.
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
    q(0, Q),
    Q(N).

:- pred q(int::in, pred(int)::out(pred(in) is semidet)) is multi.

q(N, x(N)).
q(N, y(N)).

:- pred x(int::in, int::in) is semidet.

x(A, A).

:- pred y(int::in, int::in) is semidet.

y(_, -1).
