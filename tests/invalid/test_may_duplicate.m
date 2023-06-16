%---------------------------------------------------------------------------%
% vim:ts=4 sw=4 expandtab ft=mercury
%---------------------------------------------------------------------------%

:- module test_may_duplicate.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    A0 = 0,
    p1(A0, A1),
    p2(A1, A),
    io.write_int(A, !IO),
    io.nl(!IO).

:- pred p1(int::in, int::out) is det.
:- pragma no_inline(p1/2).

:- pragma foreign_proc("C",
    p1(N::in, M::out),
    [will_not_call_mercury, promise_pure, may_duplicate],
"
    M = N + 1;
").

:- pred p2(int::in, int::out) is det.
:- pragma inline(p2/2).

:- pragma foreign_proc("C",
    p2(N::in, M::out),
    [will_not_call_mercury, promise_pure, may_not_duplicate],
"
    M = N + 10;
").
