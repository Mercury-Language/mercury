%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module oracle_db.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    ( if a(99, 99, 99) then
        io.write_string("yes.\n", !IO)
    else
        io.write_string("no.\n", !IO)
    ).

:- pred a(int::in, int::in, int::in) is semidet.

a(X, Y, Z) :-
    b(X),
    b(Y),
    b(Z).

:- pred b(int::in) is semidet.

b(99).
