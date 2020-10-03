%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A regression test to ensure that we don't get warnings about the variables
% _IO0 and _IO in the foreign_proc occuring only once.
%

:- module no_warn_singleton.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    f(X, !IO),
    io.write_int(X, !IO),
    io.nl(!IO).

:- pred f(int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    f(X::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    X = 5;
").

f(5, !IO).
