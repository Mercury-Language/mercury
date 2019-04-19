%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a test of the compiler's ability to handle predicates
% in which the Mercury clauses are together, but the foreign_procs
% are separate from them.
%
%---------------------------------------------------------------------------%

:- module warn_non_contiguous_foreign_group.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.

main(!IO) :-
    test(1, !IO),
    test(3, !IO),
    test(5, !IO),
    test(7, !IO),
    test(9, !IO),
    test(11, !IO),
    test(13, !IO).

:- pred test(int::in, io::di, io::uo) is det.

test(N, !IO) :-
    ( if p(N, PM) then
        io.format("p(%d) = %d\n", [i(N), i(PM)], !IO)
    else
        io.format("p(%d) failed\n", [i(N)], !IO)
    ),
    ( if q(N, QM) then
        io.format("q(%d) = %d\n", [i(N), i(QM)], !IO)
    else
        io.format("q(%d) failed\n", [i(N)], !IO)
    ).

:- pred p(int::in, int::out) is semidet.

:- pred q(int::in, int::out) is semidet.

p(0, 1).

q(10, 11).
q(11, 12).

:- pragma foreign_proc("C",
    p(N::in, M::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (N < 10) {
        M = N + 1;
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
").

:- pragma foreign_proc("C",
    q(N::in, M::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (N < 5) {
        M = N + 1;
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
").
