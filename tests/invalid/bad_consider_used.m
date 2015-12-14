%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bad_consider_used.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(!IO) :-
    ( if p(1, B) then
        io.write_int(B, !IO),
        io.nl(!IO)
    else
        io.write_string("p failed\n", !IO)
    ).

:- pred p(int::in, int::out) is semidet.

p(A, B) :-
    A > 10,
    B = A + 1.

:- pred q(int::in, int::out) is semidet.
:- pragma consider_used(q/3).
:- pragma consider_used(g/2).

q(A, B) :-
    A > 20,
    B = A + 2.
