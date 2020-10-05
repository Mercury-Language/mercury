%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test that we can use non-mode specific mercury implementations in
% conjunction with foreign code procs.
%

:- module foreign_and_mercury.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    f(2, Y),
    io.write_int(Y, !IO),
    io.nl(!IO),
    g(2, Z),
    io.write_int(Z, !IO),
    io.nl(!IO).

:- pred f(int::in, int::out) is det.

f(X, X).
:- pragma foreign_proc(c, f(X::in, Y::out), [promise_pure],
"
    Y = X;
").

:- pred g(int::in, int::out) is det.

g(X, X).
:- pragma foreign_proc(c, g(X::in, Y::out), [promise_pure],
"
    Y = X;
").
