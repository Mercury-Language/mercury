%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module small.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    p(X),
    io.write_int(X, !IO),
    io.nl(!IO).

:- pred p(int::out) is det.

p(42).
