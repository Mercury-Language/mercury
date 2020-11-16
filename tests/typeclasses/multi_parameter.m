%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module multi_parameter.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.

:- typeclass m(A, B) where [
    pred a(A, B),
    mode a(in, out) is det
].

:- instance m(char, int) where [
    pred(a/2) is char.to_int
].

main(!IO) :-
    foo('z', X),
    io.write_int(X, !IO),
    io.nl(!IO).

:- pred foo(A::in, B::out) is det <= m(A, B).
:- pragma no_inline(foo/2).

foo(X, Y) :- a(X, Y).
