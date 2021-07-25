%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module confirm_abort.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    p(X),
    io.write_int(X, !IO),
    io.nl(!IO).

:- pred p(int::out) is det.

p(N) :-
    q(N).

:- pred q(int::out) is det.

q(27).
