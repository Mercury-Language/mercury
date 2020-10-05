%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module failure_unify.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type f
    --->    f(int).

main(!IO) :-
    ( if
        X = f(1),
        Y = f(2),
        X = Y
    then
        io.write_string("test failed\n", !IO)
    else
        io.write_string("test succeeded\n", !IO)
    ).
