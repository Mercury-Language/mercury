%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module foreign_singleton.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    f(X, !IO),
    io.write_int(X, !IO),
    io.nl(!IO),
    g(Y, !IO),
    io.write_int(Y, !IO),
    io.nl(!IO).

:- pred f(int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    f(X::out, IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    X = 5;
").

f(X, !IO).

:- pred g(int::out, io::di, io::uo) is det.

g(X, !IO).

:- pragma foreign_proc("C",
    g(X::out, IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    X = 5;
").
