%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module track_through_catch.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module exception.
:- import_module int.

main(!IO) :-
    p(X),
    try(q(X), Result),
    io.write_line(Result, !IO).

:- pred p(int::out) is det.

p(2).

:- pred q(int::in, int::out) is det.

q(X, Y) :-
    r(X, Y).

:- pred r(int::in, int::out) is det.

r(X, X).
