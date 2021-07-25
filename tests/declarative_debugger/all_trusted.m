%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module all_trusted.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(!IO) :-
    p(1, X),
    io.write_int(X, !IO).

:- pred p(int::in, int::out) is det.

p(X, Y) :-
    q(X, Y).

:- pred q(int::in, int::out) is det.

q(X, X+1).
