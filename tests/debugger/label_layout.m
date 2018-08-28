%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module label_layout.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    ( if
        (
            a(1, X)
        ;
            a(2, X)
        ),
        X = 0
    then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ).

:- pred a(int::in, int::out) is det.

a(X, X).
