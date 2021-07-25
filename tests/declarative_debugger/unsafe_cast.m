%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module unsafe_cast.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    p(Y),
    io.write_int(Y, !IO),
    io.nl(!IO).

:- pred p(int::out) is det.

p(Y) :-
    X = 42,
    private_builtin.unsafe_type_cast(X, Y).
